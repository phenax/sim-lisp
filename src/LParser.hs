module LParser where

import Text.Parsec

--import Text.Parsec.Char (anyChar, char, digit, noneOf, spaces, string)
--import Text.Parsec.Combinator (many1, manyTill)
--import Text.Parsec.String (Parser)

data Expression
  = LInteger Integer
  | LString String
  | LList [Expression]
  | LFunction String [Expression]

instance Eq Expression where
  (==) (LInteger a) (LInteger b) = a == b
  (==) (LString a) (LString b) = a == b
  (==) (LList a) (LList b) = a == b
  (==) (LFunction a1 b1) (LFunction a2 b2) = a1 == a2 && b1 == b2
  (==) _ _ = False

instance Show Expression where
  show (LInteger n) = show n
  show (LString s) = "\"" ++ s ++ "\""
  show (LList exprs) = "(" ++ (unwords . map show $ exprs) ++ ")"
  show (LFunction op lst) = "(" ++ op ++ " " ++ (unwords . map show $ lst) ++ ")"

whitespace :: Parsec String u String
whitespace = many $ oneOf [' ', '\n', '\t']

withWhitespace :: Parsec String u a -> Parsec String u a
withWhitespace comb = do
  whitespace
  content <- comb
  whitespace
  return content

numberP :: Parsec String u Expression
numberP = LInteger . (\x -> read x :: Integer) <$> many1 digit

stringP :: Parsec String u Expression
stringP = do
  char '"'
  LString <$> anyChar `manyTill` char '"'

valueP :: Parsec String u Expression
valueP = numberP <|> stringP <?> "wtf value is this dude?"

variableNameP :: Parsec String u String
variableNameP = many1 $ letter <|> oneOf ['+', '-', '*', '/', '\'']

functionP = withWhitespace $ do
  char '('
  whitespace
  op <- variableNameP
  whitespace
  arg <- (functionP <|> valueP) `sepBy` whitespace
  whitespace
  char ')'
  return $ LFunction op arg

expressionP = withWhitespace $ functionP <|> valueP

multipleExpressionsP = withWhitespace $ expressionP `sepBy` whitespace

tokenize = parse multipleExpressionsP "LithParserError"
