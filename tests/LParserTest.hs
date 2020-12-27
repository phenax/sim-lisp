{-# LANGUAGE QuasiQuotes #-}

module LParserTest where

import Atom
import LParser
import Test.Hspec
import Text.Parsec
import Text.RawString.QQ

sym = Atom . AtomSymbol

valueParsers = do
  let parseValue = parse stringP "Str"
   in describe "stringP" $ do
        it "should parse simple string" $ do
          parseValue "\"hello world\"" `shouldBe` Right ((Atom . AtomString) "hello world")
        it "should parse empty string" $ do
          parseValue "\"\"" `shouldBe` Right ((Atom . AtomString) "")
        it "should parse string with all characters" $ do
          parseValue "\"121lk~!@#$%^&*()_+-=`[]\\;',./\"" `shouldBe` Right ((Atom . AtomString) "121lk~!@#$%^&*()_+-=`[]\\;',./")
        it "should return error for empty" $ do
          True `shouldBe` case parseValue "" of
            Right _ -> False
            Left _ -> True

  let parseValue = parse numberP "Num"
   in describe "numberP" $ do
        it "should parse number" $ do
          parseValue "9182323" `shouldBe` Right ((Atom . AtomInt) 9182323)
        it "should get number till non digit character" $ do
          parseValue "918a2323" `shouldBe` Right ((Atom . AtomInt) 918)

  let parseValue = parse listP "List"
   in describe "listP" $ do
        it "should parse simple list" $ do
          parseValue "'( 1 2 4 6 \"abc\" )" `shouldBe` Right (Atom . AtomList $ [(Atom . AtomInt) 1, (Atom . AtomInt) 2, (Atom . AtomInt) 4, (Atom . AtomInt) 6, (Atom . AtomString) "abc"])
        it "should parse list of expressions" $ do
          parseValue "'( 1 (+ 5 2) 4 (/ 12 6) )"
            `shouldBe` Right
              ( Atom . AtomList $
                  [ (Atom . AtomInt) 1,
                    SymbolExpression [sym "+", (Atom . AtomInt) 5, (Atom . AtomInt) 2],
                    (Atom . AtomInt) 4,
                    SymbolExpression [sym "/", (Atom . AtomInt) 12, (Atom . AtomInt) 6]
                  ]
              )

  let parseValue = parse symbolP "Symbol"
   in describe "symbolP" $ do
        it "should parse words to symbol" $ do
          parseValue "hello" `shouldBe` Right (sym "hello")
          parseValue "world" `shouldBe` Right (sym "world")
          parseValue "nice" `shouldBe` Right (sym "nice")
          parseValue "Tit" `shouldBe` Right (sym "Tit")
          parseValue "Fuck" `shouldBe` Right (sym "Fuck")
        it "should allow numbers" $ do
          parseValue "hello121world" `shouldBe` Right (sym "hello121world")
          parseValue "121world" `shouldBe` Right (sym "121world")
        it "should allow special characters" $ do
          parseValue "he+llo" `shouldBe` Right (sym "he+llo")
          parseValue "wor-l*!!!d" `shouldBe` Right (sym "wor-l*!!!d")
          parseValue "nice?" `shouldBe` Right (sym "nice?")
          parseValue "+" `shouldBe` Right (sym "+")

  let parseValue = parse booleanP "Boolean"
   in describe "booleanP" $ do
        it "should parse T and F to booleans" $ do
          parseValue "T" `shouldBe` Right ((Atom . AtomBool) True)
          parseValue "F" `shouldBe` Right ((Atom . AtomBool) False)

expressionParsers = do
  let parseValue = parse expressionP "Expr"
   in describe "expressionP" $ do
        it "should parse function call with one argument" $ do
          parseValue "1" `shouldBe` Right ((Atom . AtomInt) 1)
        it "should parse function call with one argument" $ do
          parseValue "\"hello world\"" `shouldBe` Right ((Atom . AtomString) "hello world")
        it "should parse function call with one argument" $ do
          parseValue "(+ 1)" `shouldBe` Right (SymbolExpression [sym "+", (Atom . AtomInt) 1])
        it "should parse function call with multiple arguments" $ do
          parseValue "(+ 1 2)" `shouldBe` Right (SymbolExpression [sym "+", (Atom . AtomInt) 1, (Atom . AtomInt) 2])
        it "should parse named function call" $ do
          parseValue "(incrementBy 1 20)" `shouldBe` Right (SymbolExpression [sym "incrementBy", (Atom . AtomInt) 1, (Atom . AtomInt) 20])
        it "should parse nested expressions" $ do
          parseValue "(+ 1 (- 3 2))" `shouldBe` Right (SymbolExpression [sym "+", (Atom . AtomInt) 1, SymbolExpression [sym "-", (Atom . AtomInt) 3, (Atom . AtomInt) 2]])
        it "should parse with any whitespace" $ do
          parseValue " (\t+    1 (  \n  - 3  \t  2))" `shouldBe` Right (SymbolExpression [sym "+", (Atom . AtomInt) 1, SymbolExpression [sym "-", (Atom . AtomInt) 3, (Atom . AtomInt) 2]])
        it "should parse booleans" $ do
          parseValue "(fn T F)" `shouldBe` Right (SymbolExpression [Atom (AtomSymbol "fn"), Atom . AtomBool $ True, Atom . AtomBool $ False])

  let parseValue = parse multipleExpressionsP "MultipleExpr"
   in describe "multipleExpressionsP" $ do
        it "should parse multiple expressions" $ do
          parseValue "(+ 1 (- 3 2)) (+ 3 (* 9 6) 5)"
            `shouldBe` Right
              [ SymbolExpression [sym "+", (Atom . AtomInt) 1, SymbolExpression [sym "-", (Atom . AtomInt) 3, (Atom . AtomInt) 2]],
                SymbolExpression [sym "+", (Atom . AtomInt) 3, SymbolExpression [sym "*", (Atom . AtomInt) 9, (Atom . AtomInt) 6], (Atom . AtomInt) 5]
              ]
        it "should parse comments out" $ do
          parseValue
            [r|
            ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
            (declare x 20) ; Extra commenty
            (declare y 10) ; Extra commenty
            ; hello world
            (+ x y) ; cool
            ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          |]
            `shouldBe` Right
              [ SymbolExpression [sym "do"],
                SymbolExpression [sym "declare", sym "x", Atom $ AtomInt 20],
                SymbolExpression [sym "do"],
                SymbolExpression [sym "declare", sym "y", Atom $ AtomInt 10],
                SymbolExpression [sym "do"],
                SymbolExpression [sym "do"],
                SymbolExpression [sym "+", sym "x", sym "y"],
                SymbolExpression [sym "do"],
                SymbolExpression [sym "do"]
              ]
