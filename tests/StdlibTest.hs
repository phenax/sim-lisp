{-# LANGUAGE QuasiQuotes #-}

module StdlibTest where

import Atom
import Control.Monad
import Control.Monad.Trans.Except
import Errors
import Eval
import LParser
import Test.Hspec
import Text.RawString.QQ

listExpression = AtomSymbol . SymbolExpression

tests = do
  let eval = runExceptT . (evaluate <=< (except . tokenize))
   in describe "stdlib" $ do
        describe "core#math" $ do
          it "should be identity for single args" $ do
            eval "(+ 5)" `shouldReturn` Right (AtomInt 5)
            eval "(- 5)" `shouldReturn` Right (AtomInt (-5))
            eval "(* 7)" `shouldReturn` Right (AtomInt 7)
            eval "(/ 12)" `shouldReturn` Right (AtomInt 12)
          it "should do basic 2 value math" $ do
            eval "(+ 5 2)" `shouldReturn` Right (AtomInt 7)
            eval "(+ 120 5)" `shouldReturn` Right (AtomInt 125)
            eval "(- 120 5)" `shouldReturn` Right (AtomInt 115)
            eval "(* 26 2)" `shouldReturn` Right (AtomInt 52)
            eval "(/ 26 2)" `shouldReturn` Right (AtomInt 13)
            eval "(% 10 2)" `shouldReturn` Right (AtomInt 0)
            eval "(% 10 3)" `shouldReturn` Right (AtomInt 1)
            eval "(% 13 5)" `shouldReturn` Right (AtomInt 3)
            eval "(% 3 3)" `shouldReturn` Right (AtomInt 0)
          it "should do nested computations" $ do
            eval "(* 5 (+ 2) (- 11 2) (/ 10 5))" `shouldReturn` Right (AtomInt 180)
          it "should do basic math for n-args" $ do
            eval "(+ 10 2 3 6)" `shouldReturn` Right (AtomInt 21)
            eval "(+ 120 5 21 1 1 6)" `shouldReturn` Right (AtomInt 154)
            eval "(- 120 5)" `shouldReturn` Right (AtomInt 115)
            eval "(- 120 5 12 4 1 0 0 9)" `shouldReturn` Right (AtomInt 89)
            eval "(/ 500 5 2 1 1 5)" `shouldReturn` Right (AtomInt 10)
            eval "(* 26 2 10)" `shouldReturn` Right (AtomInt 520)
            eval "(/ 26 2)" `shouldReturn` Right (AtomInt 13)
            eval "(/ 500 5 2 1 1 5)" `shouldReturn` Right (AtomInt 10)
          it "should emit eval error for invalid types" $ do
            eval "(+ 10 \"fucking hell\" 5)" `shouldReturn` Left (EvalError "Invalid set of params")
            eval "(+ 10 (+ 12 \"1\"))" `shouldReturn` Left (EvalError "Invalid set of params")
          it "should allow overriding default operators" $ do
            eval "(def + (a b) (* a b)) (+ 5 3)" `shouldReturn` Right (AtomInt 15)
          it "should compare ints" $ do
            eval "(= 6 5 4 3 2 1)" `shouldReturn` Right (AtomBool False)
            eval "(= 5 5)" `shouldReturn` Right (AtomBool True)
            eval "(= 5 2)" `shouldReturn` Right (AtomBool False)
            eval "(= 6 6 6 6 6)" `shouldReturn` Right (AtomBool True)
            eval "(= 6 6 5 6 6)" `shouldReturn` Right (AtomBool False)
            eval "(= 6 6 5 6 Nil)" `shouldReturn` Right (AtomBool False)
            eval "(< 5 1)" `shouldReturn` Right (AtomBool False)
            eval "(< 1 5)" `shouldReturn` Right (AtomBool True)
            eval "(> 5 1)" `shouldReturn` Right (AtomBool True)
            eval "(> 5 5)" `shouldReturn` Right (AtomBool False)
            eval "(>= 5 5)" `shouldReturn` Right (AtomBool True)
            eval "(>= 5 2)" `shouldReturn` Right (AtomBool True)
            eval "(< 5 5)" `shouldReturn` Right (AtomBool False)
            eval "(<= 5 5)" `shouldReturn` Right (AtomBool True)
            eval "(<= 2 5)" `shouldReturn` Right (AtomBool True)

        let fnDef =
              [r|
            (def getval (value) (cond
              '((= 4 value)  "four")
              '((= 5 value)  1 2 "five")
              '((= 2 value)  1 "two")
              '(else         "default")
            ))
          |]
         in describe "core#cond" $ do
              it "should return the value for condition" $ do
                eval (fnDef ++ " (getval 4)") `shouldReturn` Right (AtomString "four")
              it "should return else value if no condition matches" $ do
                eval (fnDef ++ " (getval 99)") `shouldReturn` Right (AtomString "default")
              it "should return the last value for matching case" $ do
                eval (fnDef ++ " (getval 5)") `shouldReturn` Right (AtomString "five")

        describe "core#curry" $ do
          it "should partially apply a function with 1 arg" $ do
            eval "(def add10 (curry + 10)) (add10 1 3 8)" `shouldReturn` Right (AtomInt 22)
          it "should partially apply a function with multiple args arg" $ do
            eval "(def add15 (curry + 10 5)) (add15 1 3 8)" `shouldReturn` Right (AtomInt 27)
          it "should allow passing multiple curry calls" $ do
            eval "((curry + 6) ((curry * 2) 10))" `shouldReturn` Right (AtomInt 26)
          -- TODO: Fix after closure issue fixed
          xit "should allow passing curried functions as args for composition" $ do
            eval
              [r|
              (def domath (compose2 (curry + 10) (curry * 5)))
              (domath 4)
            |]
              `shouldReturn` Right (AtomInt 27)

        describe "core#compose" $ do
          it "should compose 2 functions (compose2)" $ do
            eval
              [r|
              (def normalize (x) (+ 5 x))
              (def is-adult (x) (do (display x) (> x 18)))

              (def fn (compose2 is-adult normalize))
              (fn 15)
            |]
              `shouldReturn` Right (AtomBool True)
          it "should compose 2 functions with list data type (compose2)" $ do
            eval
              [r|
              (def is-adult (x) (do (display x) (> x 18)))
              (def getage (user) (car (cdr user)))

              (def is-adult (compose2 is-adult getage))
              (is-adult '("Akshay" 20))
            |]
              `shouldReturn` Right (AtomBool True)
          xit "should compose 2 functions with list data type (compose2)" $ do
            eval
              [r|
              (def normalize (x) (+ 5 x))
              (def is-adult (x) (do (display x) (> x 18)))
              (def getage (x) (car (cdr x)))

              (def fn1 (user) (compose2 normalize getage))
              (def is-adult (compose2 (const "wow") fn1))
              (is-adult '("Akshay" 20))
            |]
              `shouldReturn` Right (AtomBool True)

        describe "core#and" $ do
          it "should and values" $ do
            eval "(and T T)" `shouldReturn` Right (AtomBool True)
            eval "(and T F)" `shouldReturn` Right (AtomBool False)
            eval "(and F T)" `shouldReturn` Right (AtomBool False)
            eval "(and F F)" `shouldReturn` Right (AtomBool False)
            eval "(and T)" `shouldReturn` Right (AtomBool True)
            eval "(and F)" `shouldReturn` Right (AtomBool False)
            eval "(and T T T T T)" `shouldReturn` Right (AtomBool True)
            eval "(and T F T T T)" `shouldReturn` Right (AtomBool False)
            eval "(and T T T F T)" `shouldReturn` Right (AtomBool False)
        describe "core#or" $ do
          it "should and values" $ do
            eval "(or T T)" `shouldReturn` Right (AtomBool True)
            eval "(or T F)" `shouldReturn` Right (AtomBool True)
            eval "(or F T)" `shouldReturn` Right (AtomBool True)
            eval "(or F F)" `shouldReturn` Right (AtomBool False)
            eval "(or T)" `shouldReturn` Right (AtomBool True)
            eval "(or F)" `shouldReturn` Right (AtomBool False)
            eval "(or F F F F F)" `shouldReturn` Right (AtomBool False)
            eval "(or F T F F F)" `shouldReturn` Right (AtomBool True)
            eval "(or F F F T F)" `shouldReturn` Right (AtomBool True)
        describe "core#not" $ do
          it "should return F for T and T for F and T for Nil" $ do
            eval "(not F)" `shouldReturn` Right (AtomBool True)
            eval "(not T)" `shouldReturn` Right (AtomBool False)
            eval "(not Nil)" `shouldReturn` Right (AtomBool True)
          it "should work for expressions" $ do
            eval "(not (= 5 2))" `shouldReturn` Right (AtomBool True)
          it "should be false for other atoms" $ do
            eval "(not 50)" `shouldReturn` Right (AtomBool False)
            eval "(not 0)" `shouldReturn` Right (AtomBool False)
            eval "(not \"hellow\")" `shouldReturn` Right (AtomBool False)
        describe "core#max, core#min" $ do
          it "should return max value" $ do
            eval "(max 1 2 3 4 5)" `shouldReturn` Right (AtomInt 5)
            eval "(max 5 4 3 2 1)" `shouldReturn` Right (AtomInt 5)
            eval "(max 3 9 1)" `shouldReturn` Right (AtomInt 9)
            eval "(max (- 3) (- 2) (- 9) (- 1))" `shouldReturn` Right (AtomInt (-1))
            eval "(max 10)" `shouldReturn` Right (AtomInt 10)
            eval "(max 6 6 6)" `shouldReturn` Right (AtomInt 6)
          it "should apply max to list of values" $ do
            eval "(apply max '(3 2 9 1))" `shouldReturn` Right (AtomInt 9)
            eval "(apply max '(10))" `shouldReturn` Right (AtomInt 10)
            eval "(apply max '(6 6 6))" `shouldReturn` Right (AtomInt 6)
          it "should return min value" $ do
            eval "(min 1 2 3 4 5)" `shouldReturn` Right (AtomInt 1)
            eval "(min 5 4 3 2 1)" `shouldReturn` Right (AtomInt 1)
            eval "(min 3 9 (- 10))" `shouldReturn` Right (AtomInt (-10))
            eval "(min 10)" `shouldReturn` Right (AtomInt 10)
            eval "(min 6 6 6)" `shouldReturn` Right (AtomInt 6)
          it "should apply min to list of values" $ do
            eval "(apply min '(3 2 9 1))" `shouldReturn` Right (AtomInt 1)
            eval "(apply min '(10))" `shouldReturn` Right (AtomInt 10)
            eval "(apply min '(6 6 6))" `shouldReturn` Right (AtomInt 6)

        describe "list#foldl" $ do
          it "should fold all items in list" $ do
            eval "(foldl - 0 '(5 4 3 2 1)))" `shouldReturn` Right (AtomInt (-15))
        describe "list#foldr" $ do
          it "should fold all items in list" $ do
            eval "(foldr - 0 '(5 4 3 2 1)))" `shouldReturn` Right (AtomInt 3)

        describe "list#concat" $ do
          it "should concat lists" $ do
            eval "(concat '(1 2) '(3 4))"
              `shouldReturn` Right
                ( listExpression
                    [ Atom . AtomInt $ 1,
                      Atom . AtomInt $ 2,
                      Atom . AtomInt $ 3,
                      Atom . AtomInt $ 4
                    ]
                )
          it "should concat nil" $ do
            eval "(concat '() '())" `shouldReturn` Right AtomNil

        describe "list#append" $ do
          it "should append 3 to list" $ do
            eval "(append '(1 2) 3)" `shouldReturn` Right (listExpression [Atom . AtomInt $ 1, Atom . AtomInt $ 2, Atom . AtomInt $ 3])
          it "should append to nil" $ do
            eval "(append '() 3)" `shouldReturn` Right (listExpression [Atom . AtomInt $ 3])

        describe "list#map" $ do
          it "should map over list" $ do
            eval "(map (lambda (x) (+ x 1)) '(5 4 3)))"
              `shouldReturn` Right
                ( listExpression
                    [ Atom . AtomInt $ 6,
                      Atom . AtomInt $ 5,
                      Atom . AtomInt $ 4
                    ]
                )
          it "should return Nil for empty list" $ do
            eval "(map (lambda (x) (+ x 1)) '())" `shouldReturn` Right AtomNil

        describe "list#filter" $ do
          it "should filter list" $ do
            eval "(filter (lambda (x) (<= x 10)) '(20 5 8 2 16))"
              `shouldReturn` Right
                ( listExpression
                    [ Atom . AtomInt $ 5,
                      Atom . AtomInt $ 8,
                      Atom . AtomInt $ 2
                    ]
                )
          it "should return Nil for empty list" $ do
            eval "(filter (lambda (x) (>= x 1)) '())" `shouldReturn` Right AtomNil

        describe "list#null?" $ do
          it "should return false for list with any number of items" $ do
            eval "(null? (quote (1 2 3 4 5)))" `shouldReturn` Right (AtomBool False)
          it "should return true for no items" $ do
            eval "(null? Nil)" `shouldReturn` Right (AtomBool True)
            eval "(null? (quote ()))" `shouldReturn` Right (AtomBool True)

----
