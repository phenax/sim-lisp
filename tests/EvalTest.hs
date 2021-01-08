{-# LANGUAGE QuasiQuotes #-}

module EvalTest where

import Atom
import Control.Monad
import Control.Monad.Trans.Except
import Errors
import Eval
import LParser
import Test.Hspec
import Text.RawString.QQ

evalExpressionTests = do
  let -- eval :: (CIO.ConsoleIO io) => String -> io (Either Error Atom)
      eval = runExceptT . (evaluate <=< (except . tokenize))
   in describe "evalExpression" $ do
        describe "number ops" $ do
          it "should evaluate positive and negative numbers" $ do
            eval "(+ (- 5) 2)" `shouldReturn` Right (AtomInt (-3))
            eval "(mod 10 2)" `shouldReturn` Right (AtomInt 0)
            eval "(mod 10 3)" `shouldReturn` Right (AtomInt 1)
            eval "(mod 13 5)" `shouldReturn` Right (AtomInt 3)
            eval "(mod 3 3)" `shouldReturn` Right (AtomInt 0)
        describe "bool operations" $ do
          it "should compare numbers correctly" $ do
            eval "(lt? 5 1)" `shouldReturn` Right (AtomBool False)
            eval "(lt? 1 5)" `shouldReturn` Right (AtomBool True)
            eval "(gt? 5 1)" `shouldReturn` Right (AtomBool True)
            eval "(eq? 5 5)" `shouldReturn` Right (AtomBool True)
            eval "(eq? 5 2)" `shouldReturn` Right (AtomBool False)
            eval "(gt? 5 5)" `shouldReturn` Right (AtomBool False)
            eval "(gte? 5 5)" `shouldReturn` Right (AtomBool True)
            eval "(gte? 5 2)" `shouldReturn` Right (AtomBool True)
            eval "(lt? 5 5)" `shouldReturn` Right (AtomBool False)
            eval "(lte? 5 5)" `shouldReturn` Right (AtomBool True)
            eval "(lte? 2 5)" `shouldReturn` Right (AtomBool True)
          it "should compare string correctly" $ do
            eval [r|(> "hello" "hallo")|] `shouldReturn` Right (AtomBool True)
            eval [r|(> "h" "he")|] `shouldReturn` Right (AtomBool False)
            eval [r|(> "hee" "he")|] `shouldReturn` Right (AtomBool True)
            eval [r|(> "1-1" "1-0")|] `shouldReturn` Right (AtomBool True)
            eval [r|(< "1-0" "1-1")|] `shouldReturn` Right (AtomBool True)
            eval [r|(= "1-1" "1-1")|] `shouldReturn` Right (AtomBool True)
            eval [r|(= "1-0" "1-1")|] `shouldReturn` Right (AtomBool False)
          it "should return T for all numbers, F for others" $ do
            eval "(number? 20)" `shouldReturn` Right (AtomBool True)
            eval "(number? (* 2 3))" `shouldReturn` Right (AtomBool True)
            eval "(number? 0)" `shouldReturn` Right (AtomBool True)
            eval "(declare var 20) (number? var)" `shouldReturn` Right (AtomBool True)
            eval "(declare var F) (number? var)" `shouldReturn` Right (AtomBool False)
            eval "(number? T)" `shouldReturn` Right (AtomBool False)
            eval "(number? Nil)" `shouldReturn` Right (AtomBool False)
            eval "(number? (lambda (x) (+ 5 x)))" `shouldReturn` Right (AtomBool False)
            eval "(number? \"hello\")" `shouldReturn` Right (AtomBool False)
          it "should return T for all booleans, F for others" $ do
            eval "(declare var F) (boolean? var)" `shouldReturn` Right (AtomBool True)
            eval "(boolean? T)" `shouldReturn` Right (AtomBool True)
            eval "(boolean? Nil)" `shouldReturn` Right (AtomBool True)
            eval "(boolean? (or T F))" `shouldReturn` Right (AtomBool True)
            eval "(boolean? (not T))" `shouldReturn` Right (AtomBool True)
            eval "(boolean? 20)" `shouldReturn` Right (AtomBool False)
            eval "(boolean? (* 2 3))" `shouldReturn` Right (AtomBool False)
            eval "(boolean? 0)" `shouldReturn` Right (AtomBool False)
            eval "(declare var 20) (boolean? var)" `shouldReturn` Right (AtomBool False)
            eval "(boolean? (lambda (x) (+ 5 x)))" `shouldReturn` Right (AtomBool False)
            eval "(boolean? \"hello\")" `shouldReturn` Right (AtomBool False)

        describe "stdlib loaded" $ do
          it "should be identity for single args" $ do
            eval "stdlib-loaded?" `shouldReturn` Right (AtomBool True)

        describe "do" $ do
          it "should return last expression from block" $ do
            eval
              [r|
              (do
                (+ 1 2)
                (+ 3 3)
                (+ 5 6))
            |]
              `shouldReturn` Right (AtomInt 11)
          it "should return last expression from block" $ do
            eval
              [r|
              (do
                (declare x 7)
                (+ 3 3)
                (+ 5 x))
            |]
              `shouldReturn` Right (AtomInt 12)

        describe "declare" $ do
          it "should declare variable in the current scope" $ do
            eval
              [r|
              (do
                (declare x 6)
                (declare y 3)
                (* x y))
            |]
              `shouldReturn` Right (AtomInt 18)

        describe "let" $ do
          it "should provide definied variables inside the scope" $ do
            eval "(let ((x 5) (y 6)) (+ x (* y 2)))" `shouldReturn` Right (AtomInt 17)
          it "should not provide definied variables outside the scope" $ do
            eval "(* 20 (let ((x 5)) x) x)" `shouldReturn` Left (EvalError "Variable x not found in scope")
          it "should allow let inside let definitions" $ do
            eval
              [r|(let (
                (x 5)
                (y (let ((beam 200)) (/ beam 10) ))
              ) (+ x (* y 2)))
            |]
              `shouldReturn` Right (AtomInt 45)
          it "should allow nesting let statements" $ do
            eval
              [r|(let (
                (x 5)
              ) (let ( (y 2) ) (* x y 2) ))
            |]
              `shouldReturn` Right (AtomInt 20)
          it "should allow shadowing let values" $ do
            eval "(let ((x 5)) (let ((x 2)) x))" `shouldReturn` Right (AtomInt 2)
          it "should allow using bindings in the next variable" $ do
            eval "(let ((a 1) (b (+ a 1))) (+ a b))" `shouldReturn` Right (AtomInt 3)

        describe "if" $ do
          it "should run the then body in true" $ do
            eval "(if T 1 0)" `shouldReturn` Right (AtomInt 1)
          it "should run the else body in false" $ do
            eval "(if F 1 0)" `shouldReturn` Right (AtomInt 0)
          it "should evaluate expressions that return values" $ do
            eval "(declare x 2) (if (< x 1) 1 (- 1 1))" `shouldReturn` Right (AtomInt 0)
            eval "(declare x 2) (if (= x 2) (- 5 2) (+ 20 5))" `shouldReturn` Right (AtomInt 3)

        describe "lambda" $ do
          it "should save lambda in scope and call it" $ do
            eval "(let ((incr (lambda (x) (+ x 1)))) (incr 5) )" `shouldReturn` Right (AtomInt 6)
          it "should allow inline lambda functions" $ do
            eval "((lambda (x) (+ x 1)) 10)" `shouldReturn` Right (AtomInt 11)
          it "should do factorial" $ do
            eval
              [r|(declare fact (lambda (x) (
                  if (<= x 2)
                    x
                    (* x (fact (- x 1)))
                )))
                (fact 5)
              |]
              `shouldReturn` Right (AtomInt 120)
          it "should create a lambda atom" $ do
            eval "(lambda (x) (+ x 1))"
              `shouldReturn` Right
                ( AtomLambda
                    ["x"]
                    ( SymbolExpression [createLabel "+", createLabel "x", Atom (AtomInt 1)]
                    )
                )
          it "should multiple lambdas" $ do
            eval
              [r|(let (
                (mul5 (lambda (x) (* x 5)))
                (incr (lambda (x) (+ x 1)))
              ) (incr (mul5 3)))
              |]
              `shouldReturn` Right (AtomInt 16)
          it "should shadow any variables outside scope" $ do
            eval
              [r|(declare x 100)
                (declare incr (lambda (x) (+ x 1)))
                (incr 10)
              |]
              `shouldReturn` Right (AtomInt 11)
          it "should create a lambda atom" $ do
            eval "(lambda (x) (+ x 1))"
              `shouldReturn` Right
                ( AtomLambda
                    ["x"]
                    ( SymbolExpression [createLabel "+", createLabel "x", Atom (AtomInt 1)]
                    )
                )
          it "should allow 0 parameters" $ do
            eval "(def fn () (* 2 5)) (fn)" `shouldReturn` Right (AtomInt 10)
          it "should allow accessing the rest of the params with ... symbol syntax" $ do
            eval "((lambda (... rest) rest) 1 2 3)"
              `shouldReturn` Right (AtomSymbol . SymbolExpression $ [Atom . AtomInt $ 1, Atom . AtomInt $ 2, Atom . AtomInt $ 3])
            eval "((lambda (a b ... rest) rest) 1 2 3)"
              `shouldReturn` Right (AtomSymbol . SymbolExpression $ [Atom . AtomInt $ 3])
            eval "(def tail (a ... rest) rest) (tail 1 2 3)"
              `shouldReturn` Right
                ( AtomSymbol . SymbolExpression $
                    [ Atom . AtomInt $ 2,
                      Atom . AtomInt $ 3
                    ]
                )
            eval "(def tail (a b ... rest) rest) (tail 1 2 3)"
              `shouldReturn` Right (AtomSymbol . SymbolExpression $ [Atom . AtomInt $ 3])
            eval "(def tail (... rest) rest) (tail 1 2 3)"
              `shouldReturn` Right (AtomSymbol . SymbolExpression $ [Atom . AtomInt $ 1, Atom . AtomInt $ 2, Atom . AtomInt $ 3])

        describe "quote" $ do
          it "should wrap the symbol" $ do
            eval [r|(quote hello)|] `shouldReturn` Right (AtomSymbol (createLabel "hello"))
          it "should wrap the expression" $ do
            eval [r|(quote (+ 5 2))|] `shouldReturn` Right (AtomSymbol (SymbolExpression [createLabel "+", Atom (AtomInt 5), Atom (AtomInt 2)]))

        describe "eval" $ do
          it "should evaluate a quote" $ do
            eval [r| (declare hello 6) (eval (quote (+ hello 5))) |] `shouldReturn` Right (AtomInt 11)
          it "should evaluate a quote" $ do
            eval [r| (declare fir (quote (+ 20 5))) (eval (if T fir 5)) |] `shouldReturn` Right (AtomInt 25)
          it "should return error if not a quote" $ do
            eval [r| (eval 100) |] `shouldReturn` Left (EvalError "Invalid argument passed to `eval`")
          it "should return error if no args or extra args" $ do
            eval [r| (eval) |] `shouldReturn` Left (EvalError "Invalid number of arguments to `eval`")
            eval [r| (eval 10 20 30) |] `shouldReturn` Left (EvalError "Invalid number of arguments to `eval`")

        describe "car" $ do
          it "should return first item" $ do
            eval [r| (declare hello (quote (5 4 3 2 1))) (car hello) |] `shouldReturn` Right (AtomInt 5)
          it "should return nil for empty list" $ do
            eval [r| (declare hello (quote ())) (car hello) |] `shouldReturn` Right AtomNil
          it "should throw error for non-list args" $ do
            eval [r| (car 121212) |] `shouldReturn` Left (EvalError "Invalid argument passed to `car`")
          it "should throw error for wrong number of args" $ do
            eval [r| (car 1 2 3) |] `shouldReturn` Left (EvalError "Invalid number of arguments passed to `car`")
            eval [r| (car) |] `shouldReturn` Left (EvalError "Invalid number of arguments passed to `car`")

        describe "cdr" $ do
          it "should return rest of the list" $ do
            eval [r| (declare hello (quote (5 4 3 2 1))) (cdr hello) |]
              `shouldReturn` Right (AtomSymbol (SymbolExpression [Atom (AtomInt 4), Atom (AtomInt 3), Atom (AtomInt 2), Atom (AtomInt 1)]))
          it "should return nil for empty list" $ do
            eval [r| (declare hello (quote ())) (cdr hello) |] `shouldReturn` Right AtomNil
          it "should throw error for non-list args" $ do
            eval [r| (cdr 121212) |] `shouldReturn` Left (EvalError "Invalid argument passed to `cdr`")
          it "should throw error for wrong number of args" $ do
            eval [r| (cdr 1 2 3) |] `shouldReturn` Left (EvalError "Invalid number of arguments passed to `cdr`")
            eval [r| (cdr) |] `shouldReturn` Left (EvalError "Invalid number of arguments passed to `cdr`")

        describe "cons" $ do
          it "should prepend item to list" $ do
            eval [r|(cons 5 (quote (4 3 2 1))) |]
              `shouldReturn` Right (AtomSymbol (SymbolExpression [Atom (AtomInt 5), Atom (AtomInt 4), Atom (AtomInt 3), Atom (AtomInt 2), Atom (AtomInt 1)]))
          it "should return list with first item if cdr is empty" $ do
            eval [r|(cons 5 (quote ())) |] `shouldReturn` Right (AtomSymbol (SymbolExpression [Atom (AtomInt 5)]))
          it "should throw error for non-list args" $ do
            eval [r| (cons 8 5) |] `shouldReturn` Left (EvalError "Invalid argument passed to `cons`")
          it "should throw error for wrong number of args" $ do
            eval [r| (cons 1 2 3) |] `shouldReturn` Left (EvalError "Invalid number of arguments passed to `cons`")
            eval [r| (cons) |] `shouldReturn` Left (EvalError "Invalid number of arguments passed to `cons`")

        describe "def" $ do
          it "should do factorial with def expression" $ do
            eval
              [r|(def fact (x) (
                  if (<= x 2)
                    x
                    (* x (fact (- x 1)))
                ))

               (fact 5)
              )"|]
              `shouldReturn` Right (AtomInt 120)

        describe "display" $ do
          it "should return nil" $ do
            eval "(display 1 \"hello\" 3 4)" `shouldReturn` Right AtomNil

        describe "apply" $ do
          it "should sum list of numbers" $ do
            eval "(apply + '(1 4 5 2))" `shouldReturn` Right (AtomInt 12)
          it "should check if eq" $ do
            eval "(apply eq? '(5 5))" `shouldReturn` Right (AtomBool True)
          it "should evaluate value first" $ do
            eval "(apply eq? (concat '(5) '(5)))" `shouldReturn` Right (AtomBool True)

--
