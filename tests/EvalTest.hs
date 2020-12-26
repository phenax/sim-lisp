module EvalTest where

import Control.Monad
import Errors
import Eval
import LParser
import Test.Hspec
import Text.Parsec

evalExpressionTests = do
  let eval = evalExpression emptyScope <=< (withParseErr . parse expressionP "Expr")
   in describe "evalExpression" $ do
        it "should be identity for single args" $ do
          eval "(+ 5)" `shouldBe` Right (EInteger 5)
          eval "(- 6)" `shouldBe` Right (EInteger 6)
          eval "(* 7)" `shouldBe` Right (EInteger 7)
          eval "(/ 8)" `shouldBe` Right (EInteger 8)
        it "should do basic 2 value math" $ do
          eval "(+ 5 2)" `shouldBe` Right (EInteger 7)
          eval "(+ 120 5)" `shouldBe` Right (EInteger 125)
          eval "(- 120 5)" `shouldBe` Right (EInteger 115)
          eval "(* 26 2)" `shouldBe` Right (EInteger 52)
          eval "(/ 26 2)" `shouldBe` Right (EInteger 13)
        it "should do nested computations" $ do
          eval "(* 5 (+ 2) (- 11 2) (/ 10 5))" `shouldBe` Right (EInteger 180)
        it "should do basic math for n-args" $ do
          eval "(+ 10 2 3 6)" `shouldBe` Right (EInteger 21)
          eval "(+ 120 5 21 1 1 6)" `shouldBe` Right (EInteger 154)
          eval "(- 120 5 100)" `shouldBe` Right (EInteger 15)
          eval "(* 26 2 10)" `shouldBe` Right (EInteger 520)
          eval "(/ 26 2 13)" `shouldBe` Right (EInteger 1)
        it "should emit eval error for invalid types" $ do
          eval "(+ 10 \"fucking hell\" 5)" `shouldBe` Left (EvalError "Invalid set of params")
          eval "(+ 10 (+ 12 \"1\"))" `shouldBe` Left (EvalError "Invalid set of params")

        describe "let" $ do
          xit "should do stuff" $ do
            eval "(let ((x 5) (y 6)) (+ x (* y 2)))" `shouldBe` Left (EvalError "Invalid set of params")

        describe "lambda" $ do
          xit "should do stuff" $ do
            eval "(lambda (x) (+ x 1))" `shouldBe` Left (EvalError "Invalid set of params")
