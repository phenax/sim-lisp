module LParserTest where

import LParser
import Test.Hspec
import Text.Parsec

parseValue = parse valueParser "LithParserError"

valueParsers = do
  describe "stringP" $ do
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

  describe "numberP" $ do
    it "should parse number" $ do
      parseValue "9182323" `shouldBe` Right (LInteger 9182323)

    it "should get number till non digit character" $ do
      parseValue "918a2323" `shouldBe` Right (LInteger 918)
