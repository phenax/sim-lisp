module LParser where

import Text.Parsec

--import Text.Parsec.Char (anyChar, char, digit, noneOf, spaces, string)
--import Text.Parsec.Combinator (many1, manyTill)
--import Text.Parsec.String (Parser)

data Expression
  = LInteger Integer
  | LString String
  | LFunction ([Expression] -> Expression)

instance Eq Expression where
  (==) (LInteger a) (LInteger b) = a == b
  (==) (LString a) (LString b) = a == b
  (==) _ _ = False

instance Show Expression where
  show (LInteger n) = show n
  show (LString s) = "\"" ++ s ++ "\""

numberP :: Parsec String u Expression
numberP = LInteger . (\x -> read x :: Integer) <$> many1 digit

stringP :: Parsec String u Expression
stringP = do
  char '"'
  LString <$> anyChar `manyTill` char '"'

variableName :: Parsec String u String
variableName = do
  many1 $ letter <|> char '\''

--functionParser

valueParser = do
  spaces
  content <-
    stringP
      <|> numberP
  spaces
  return content

tokenize = parse valueParser "LithParserError"
