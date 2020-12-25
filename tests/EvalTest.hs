module EvalTest where

import Control.Monad
import Errors
import Eval
import LParser
import Test.Hspec
import Text.Parsec

evalExpressionTests = do
  let eval = evalExpression <=< (withParseErr . parse expressionP "Expr")
   in describe "evalExpression" $ do
        it "should do basic 2 value math" $ do
          eval "(+ 5 2)" `shouldBe` Right (EInteger 7)
          eval "(+ 120 5)" `shouldBe` Right (EInteger 125)
          eval "(- 120 5)" `shouldBe` Right (EInteger 115)
          eval "(* 26 2)" `shouldBe` Right (EInteger 52)
          eval "(/ 26 2)" `shouldBe` Right (EInteger 13)
