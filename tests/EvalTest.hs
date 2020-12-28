{-# LANGUAGE QuasiQuotes #-}

module EvalTest where

import Atom
import Control.Monad
import Errors
import Eval
import LParser
import Test.Hspec
import Text.RawString.QQ

evalExpressionTests = do
  let eval = evaluate <=< tokenize
   in describe "evalExpression" $ do
        it "should be identity for single args" $ do
          eval "(+ 5)" `shouldBe` Right (AtomInt 5)
          eval "(- 6)" `shouldBe` Right (AtomInt 6)
          eval "(* 7)" `shouldBe` Right (AtomInt 7)
          eval "(/ 8)" `shouldBe` Right (AtomInt 8)
        it "should do basic 2 value math" $ do
          eval "(+ 5 2)" `shouldBe` Right (AtomInt 7)
          eval "(+ 120 5)" `shouldBe` Right (AtomInt 125)
          eval "(- 120 5)" `shouldBe` Right (AtomInt 115)
          eval "(* 26 2)" `shouldBe` Right (AtomInt 52)
          eval "(/ 26 2)" `shouldBe` Right (AtomInt 13)
        it "should do nested computations" $ do
          eval "(* 5 (+ 2) (- 11 2) (/ 10 5))" `shouldBe` Right (AtomInt 180)
        it "should do basic math for n-args" $ do
          eval "(+ 10 2 3 6)" `shouldBe` Right (AtomInt 21)
          eval "(+ 120 5 21 1 1 6)" `shouldBe` Right (AtomInt 154)
          eval "(- 120 5 100)" `shouldBe` Right (AtomInt 15)
          eval "(* 26 2 10)" `shouldBe` Right (AtomInt 520)
          eval "(/ 26 2 13)" `shouldBe` Right (AtomInt 1)
        it "should emit eval error for invalid types" $ do
          eval "(+ 10 \"fucking hell\" 5)" `shouldBe` Left (EvalError "Invalid set of params")
          eval "(+ 10 (+ 12 \"1\"))" `shouldBe` Left (EvalError "Invalid set of params")

        describe "bool operations" $ do
          it "should compare numbers correctly" $ do
            eval "(< 5 1)" `shouldBe` Right (AtomBool False)
            eval "(< 1 5)" `shouldBe` Right (AtomBool True)
            eval "(> 5 1)" `shouldBe` Right (AtomBool True)
            eval "(= 5 5)" `shouldBe` Right (AtomBool True)
            eval "(= 5 2)" `shouldBe` Right (AtomBool False)
            eval "(> 5 5)" `shouldBe` Right (AtomBool False)
            eval "(>= 5 5)" `shouldBe` Right (AtomBool True)
            eval "(>= 5 2)" `shouldBe` Right (AtomBool True)
            eval "(< 5 5)" `shouldBe` Right (AtomBool False)
            eval "(<= 5 5)" `shouldBe` Right (AtomBool True)
            eval "(<= 2 5)" `shouldBe` Right (AtomBool True)
          it "should compare string correctly" $ do
            eval [r|(> "hello" "hallo")|] `shouldBe` Right (AtomBool True)
            eval [r|(> "h" "he")|] `shouldBe` Right (AtomBool False)
            eval [r|(> "hee" "he")|] `shouldBe` Right (AtomBool True)
            eval [r|(> "1-1" "1-0")|] `shouldBe` Right (AtomBool True)
            eval [r|(< "1-0" "1-1")|] `shouldBe` Right (AtomBool True)
            eval [r|(= "1-1" "1-1")|] `shouldBe` Right (AtomBool True)
            eval [r|(= "1-0" "1-1")|] `shouldBe` Right (AtomBool False)

        describe "do" $ do
          it "should return last expression from block" $ do
            eval
              [r|
              (do
                (+ 1 2)
                (+ 3 3)
                (+ 5 6))
            |]
              `shouldBe` Right (AtomInt 11)
          it "should return last expression from block" $ do
            eval
              [r|
              (do
                (declare x 7)
                (+ 3 3)
                (+ 5 x))
            |]
              `shouldBe` Right (AtomInt 12)

        describe "declare" $ do
          it "should declare variable in the current scope" $ do
            eval
              [r|
              (do
                (declare x 6)
                (declare y 3)
                (* x y))
            |]
              `shouldBe` Right (AtomInt 18)

        describe "let" $ do
          it "should provide definied variables inside the scope" $ do
            eval "(let ((x 5) (y 6)) (+ x (* y 2)))" `shouldBe` Right (AtomInt 17)
          it "should not provide definied variables outside the scope" $ do
            eval "(* 20 (let ((x 5)) x) x)" `shouldBe` Left (EvalError "Variable x not found in scope")
          it "should allow let inside let definitions" $ do
            eval
              [r|(let (
                (x 5)
                (y (let ((beam 200)) (/ beam 10) ))
              ) (+ x (* y 2)))
            |]
              `shouldBe` Right (AtomInt 45)
          it "should allow nesting let statements" $ do
            eval
              [r|(let (
                (x 5)
              ) (let ( (y 2) ) (* x y 2) ))
            |]
              `shouldBe` Right (AtomInt 20)
          it "should allow shadowing let values" $ do
            eval "(let ((x 5)) (let ((x 2)) x))" `shouldBe` Right (AtomInt 2)

        describe "if" $ do
          it "should run the then body in true" $ do
            eval "(if T 1 0)" `shouldBe` Right (AtomInt 1)
          it "should run the else body in false" $ do
            eval "(if F 1 0)" `shouldBe` Right (AtomInt 0)
          it "should evaluate expressions that return values" $ do
            eval "(declare x 2) (if (< x 1) 1 (- 1 1))" `shouldBe` Right (AtomInt 0)
            eval "(declare x 2) (if (= x 2) (- 5 2) (+ 20 5))" `shouldBe` Right (AtomInt 3)

        describe "lambda" $ do
          it "should save lambda in scope and call it" $ do
            eval "(let ((incr (lambda (x) (+ x 1)))) (incr 5) )" `shouldBe` Right (AtomInt 6)
          it "should do factorial" $ do
            eval
              [r|(declare fact (lambda (x) (
                  if (<= x 2)
                    x
                    (* x (fact (- x 1)))
                )))

               (fact 5)
              |]
              `shouldBe` Right (AtomInt 120)
          it "should create a lambda atom" $ do
            eval "(lambda (x) (+ x 1))"
              `shouldBe` Right
                ( AtomLambda
                    ["x"]
                    ( SymbolExpression [createLabel "+", createLabel "x", Atom (AtomInt 1)]
                    )
                )
          it "should multiple lambdas" $ do
            eval
              [r|(let (
                (mul (lambda (x) (* x 5)))
                (incr (lambda (x) (+ x 1)))
              ) (incr (mul 3)))
              |]
              `shouldBe` Right (AtomInt 16)
          it "should shadow any variables outside scope" $ do
            eval
              [r|(declare x 100)
                (declare incr (lambda (x) (+ x 1)))
                (incr 10)
              |]
              `shouldBe` Right (AtomInt 11)
          it "should create a lambda atom" $ do
            eval "(lambda (x) (+ x 1))"
              `shouldBe` Right
                ( AtomLambda
                    ["x"]
                    ( SymbolExpression [createLabel "+", createLabel "x", Atom (AtomInt 1)]
                    )
                )

        describe "quote" $ do
          it "should wrap the symbol" $ do
            eval [r|(quote hello)|] `shouldBe` Right (AtomSymbol (createLabel "hello"))
          it "should wrap the expression" $ do
            eval [r|(quote (+ 5 2))|] `shouldBe` Right (AtomSymbol (SymbolExpression [createLabel "+", Atom (AtomInt 5), Atom (AtomInt 2)]))

        describe "eval" $ do
          it "should evaluate a quote" $ do
            eval [r| (declare hello 6) (eval (quote (+ hello 5))) |] `shouldBe` Right (AtomInt 11)
          it "should evaluate a quote" $ do
            eval [r| (declare fir (quote (+ 20 5))) (eval (if T fir 5)) |] `shouldBe` Right (AtomInt 25)
          it "should return error if not a quote" $ do
            eval [r| (eval 100) |] `shouldBe` Left (EvalError "Invalid argument passed to `eval`")
          it "should return error if no args or extra args" $ do
            eval [r| (eval) |] `shouldBe` Left (EvalError "Invalid number of arguments to `eval`")
            eval [r| (eval 10 20 30) |] `shouldBe` Left (EvalError "Invalid number of arguments to `eval`")

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
              `shouldBe` Right (AtomInt 120)

        describe "stdlib loaded" $ do
          it "should be identity for single args" $ do
            eval "stdlib-loaded?" `shouldBe` Right (AtomBool True)

--
