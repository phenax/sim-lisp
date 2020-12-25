module LParserTest where

import LParser
import Test.Hspec
import Text.Parsec

valueParsers = do
  let parseValue = parse stringP "LithParserError"
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

  let parseValue = parse numberP "LithParserError"
   in describe "numberP" $ do
        it "should parse number" $ do
          parseValue "9182323" `shouldBe` Right (LInteger 9182323)

        it "should get number till non digit character" $ do
          parseValue "918a2323" `shouldBe` Right (LInteger 918)

expressionParsers = do
  let parseValue = parse expressionP "LithParserError"
   in describe "expressionP - function call" $ do
        it "should parse function call with one argument" $ do
          parseValue "(+ 1)" `shouldBe` Right (LFunction "+" [LInteger 1])

        it "should parse function call with multiple arguments" $ do
          parseValue "(+ 1 2)" `shouldBe` Right (LFunction "+" [LInteger 1, LInteger 2])
