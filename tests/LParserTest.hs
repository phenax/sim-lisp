module LParserTest where

import LParser
import Test.Hspec
import Text.Parsec

valueParsers = do
  let parseValue = parse stringP "Str"
   in describe "stringP" $ do
        it "should parse simple string" $ do
          parseValue "\"hello world\"" `shouldBe` Right (LString "hello world")

        it "should parse empty string" $ do
          parseValue "\"\"" `shouldBe` Right (LString "")

        it "should parse string with all characters" $ do
          parseValue "\"121lk~!@#$%^&*()_+-=`[]\\;',./\"" `shouldBe` Right (LString "121lk~!@#$%^&*()_+-=`[]\\;',./")

        it "should return error for empty" $ do
          True `shouldBe` case parseValue "" of
            Right _ -> False
            Left _ -> True

  let parseValue = parse numberP "Num"
   in describe "numberP" $ do
        it "should parse number" $ do
          parseValue "9182323" `shouldBe` Right (LInteger 9182323)

        it "should get number till non digit character" $ do
          parseValue "918a2323" `shouldBe` Right (LInteger 918)

expressionParsers = do
  let parseValue = parse expressionP "Expr"
   in describe "expressionP - function call" $ do
        --it "should parse function call with one argument" $ do
        --parseValue "1" `shouldBe` Right (LInteger 1)

        it "should parse function call with one argument" $ do
          parseValue "(+ 1)" `shouldBe` Right (LFunction "+" [LInteger 1])

        it "should parse function call with multiple arguments" $ do
          parseValue "(+ 1 2)" `shouldBe` Right (LFunction "+" [LInteger 1, LInteger 2])

        it "should parse nested expressions" $ do
          parseValue "(+ 1 (- 3 2))" `shouldBe` Right (LFunction "+" [LInteger 1, LFunction "-" [LInteger 3, LInteger 2]])

        it "should parse with any whitespace" $ do
          parseValue "(+    1 (  \n  - 3  \t  2))" `shouldBe` Right (LFunction "+" [LInteger 1, LFunction "-" [LInteger 3, LInteger 2]])

  let parseValue = parse multipleExpressionsP "MultipleExpr"
   in describe "multipleExpressionsP" $ do
        it "should parse multiple expressions" $ do
          parseValue "(+ 1 (- 3 2)) (+ 3 (* 9 6))"
            `shouldBe` Right
              [ LFunction "+" [LInteger 1, LFunction "-" [LInteger 3, LInteger 2]],
                LFunction "+" [LInteger 3, LFunction "*" [LInteger 9, LInteger 6]]
              ]
