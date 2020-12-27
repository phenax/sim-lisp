{-# LANGUAGE QuasiQuotes #-}

module EvalTest where

import Atom
import Control.Monad
import Errors
import Eval
import LParser
import Test.Hspec
import Text.Parsec
import Text.RawString.QQ

evalExpressionTests = do
  let eval = evalExpressionPure emptyScope <=< (withParseErr . parse expressionP "Expr")
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

        describe "lambda" $ do
          it "should save lambda in scope and call it" $ do
            eval "(let ((incr (lambda (x) (+ x 1)))) (incr 5) )" `shouldBe` Right (AtomInt 6)
          it "should multiple lambdas" $ do
            eval
              [r|(let (
                (mul (lambda (x) (* x 5)))
                (incr (lambda (x) (+ x 1)))
              ) (incr (mul 3)))
            "|]
              `shouldBe` Right (AtomInt 16)
          it "should create a lambda atom" $ do
            eval "(lambda (x) (+ x 1))"
              `shouldBe` Right
                ( AtomLambda
                    ["x"]
                    ( SymbolExpression [Atom (AtomSymbol "+"), Atom (AtomSymbol "x"), Atom (AtomInt 1)]
                    )
                )

--
--
--
--
--
--
--
--
