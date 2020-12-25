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

  let parseValue = parse listP "List"
   in describe "listP" $ do
        it "should parse simple list" $ do
          parseValue "'( 1 2 4 6 \"abc\" )" `shouldBe` Right (LList [LInteger 1, LInteger 2, LInteger 4, LInteger 6, LString "abc"])

        it "should parse list of expressions" $ do
          parseValue "'( 1 (+ 5 2) 4 (/ 12 6) )"
            `shouldBe` Right
              ( LList
                  [ LInteger 1,
                    SExpression (Symbol "+") [LInteger 5, LInteger 2],
                    LInteger 4,
                    SExpression (Symbol "/") [LInteger 12, LInteger 6]
                  ]
              )

expressionParsers = do
  let parseValue = parse expressionP "Expr"
   in describe "expressionP" $ do
        it "should parse function call with one argument" $ do
          parseValue "1" `shouldBe` Right (LInteger 1)

        it "should parse function call with one argument" $ do
          parseValue "\"hello world\"" `shouldBe` Right (LString "hello world")

        it "should parse function call with one argument" $ do
          parseValue "(+ 1)" `shouldBe` Right (SExpression (Symbol "+") [LInteger 1])

        it "should parse function call with multiple arguments" $ do
          parseValue "(+ 1 2)" `shouldBe` Right (SExpression (Symbol "+") [LInteger 1, LInteger 2])

        it "should parse named function call" $ do
          parseValue "(incrementBy 1 20)" `shouldBe` Right (SExpression (Symbol "incrementBy") [LInteger 1, LInteger 20])

        it "should parse nested expressions" $ do
          parseValue "(+ 1 (- 3 2))" `shouldBe` Right (SExpression (Symbol "+") [LInteger 1, SExpression (Symbol "-") [LInteger 3, LInteger 2]])

        it "should parse with any whitespace" $ do
          parseValue " (\t+    1 (  \n  - 3  \t  2))" `shouldBe` Right (SExpression (Symbol "+") [LInteger 1, SExpression (Symbol "-") [LInteger 3, LInteger 2]])

  let parseValue = parse multipleExpressionsP "MultipleExpr"
   in describe "multipleExpressionsP" $ do
        it "should parse multiple expressions" $ do
          parseValue "(+ 1 (- 3 2)) (+ 3 (* 9 6) 5)"
            `shouldBe` Right
              [ SExpression (Symbol "+") [LInteger 1, SExpression (Symbol "-") [LInteger 3, LInteger 2]],
                SExpression (Symbol "+") [LInteger 3, SExpression (Symbol "*") [LInteger 9, LInteger 6], LInteger 5]
              ]
