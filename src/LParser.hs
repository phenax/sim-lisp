module LParser where

import Text.Parsec

--import Text.Parsec.Char (anyChar, char, digit, noneOf, spaces, string)
--import Text.Parsec.Combinator (many1, manyTill)
--import Text.Parsec.String (Parser)

data Expression
  = LInteger Integer
  | LString String
  | Symbol String
  | LList [Expression]
  | SExpression Expression [Expression] -- Find way to use Symbol inside SExpression

instance Eq Expression where
  (==) (LInteger a) (LInteger b) = a == b
  (==) (LString a) (LString b) = a == b
  (==) (Symbol a) (Symbol b) = a == b
  (==) (LList a) (LList b) = a == b
  (==) (SExpression a1 b1) (SExpression a2 b2) = a1 == a2 && b1 == b2
  (==) _ _ = False

instance Show Expression where
  show (LInteger n) = show n
  show (LString s) = "\"" ++ s ++ "\""
  show (Symbol s) = s
  show (LList exprs) = "(" ++ (unwords . map show $ exprs) ++ ")"
  show (SExpression op lst) = "(" ++ show op ++ " " ++ (unwords . map show $ lst) ++ ")"

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

listP :: Parsec String u Expression
listP = do
  char '\''
  char '('
  whitespace
  exprs <- (sExpressionP <|> valueP) `sepBy` whitespace
  whitespace
  return $ LList exprs

symbolP :: Parsec String u Expression
symbolP = fmap Symbol $ many1 $ letter <|> oneOf ['+', '-', '*', '/', '\'']

valueP :: Parsec String u Expression
valueP = withWhitespace (numberP <|> stringP <|> listP <|> symbolP <?> "wtf value is this dude?")

sExpressionP = withWhitespace $ do
  char '('
  whitespace
  op <- symbolP
  whitespace
  arg <- (valueP <|> sExpressionP) `sepBy` whitespace
  whitespace
  char ')'
  return $ SExpression op arg

expressionP = withWhitespace $ sExpressionP <|> valueP

multipleExpressionsP = withWhitespace $ expressionP `sepBy` whitespace

tokenize = parse multipleExpressionsP "LithParserError"

--
--
