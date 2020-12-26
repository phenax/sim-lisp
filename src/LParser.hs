module LParser where

import Errors
import Text.Parsec

newtype Symbol = Symbol String deriving (Show, Eq)

data Atom
  = AtomString String
  | AtomInt Integer
  | AtomBool Bool
  | AtomList [Expression]
  | AtomSymbol Symbol
  deriving (Show, Eq)

data Expression
  = Atom Atom
  | SymbolExpression Symbol [Expression]
  deriving (Eq)

instance Show Expression where
  show (Atom (AtomInt n)) = show n
  show (Atom (AtomString s)) = "\"" ++ s ++ "\""
  show (Atom (AtomList exprs)) = "(" ++ (unwords . map show $ exprs) ++ ")"
  show (Atom (AtomSymbol (Symbol s))) = s
  show (SymbolExpression op lst) = "(" ++ show op ++ " " ++ (unwords . map show $ lst) ++ ")"

whitespace :: Parsec String u String
whitespace = many $ oneOf [' ', '\n', '\t']

withWhitespace :: Parsec String u a -> Parsec String u a
withWhitespace comb = do
  whitespace
  content <- comb
  whitespace
  return content

numberP :: Parsec String u Expression
numberP = Atom . AtomInt . (\x -> read x :: Integer) <$> many1 digit

stringP :: Parsec String u Expression
stringP = do
  char '"'
  Atom . AtomString <$> anyChar `manyTill` char '"'

listP :: Parsec String u Expression
listP = do
  char '\''
  char '('
  whitespace
  exprs <- (sExpressionP <|> atomP) `sepBy` whitespace
  whitespace
  return . Atom . AtomList $ exprs

parseSymbolString :: Parsec String u String
parseSymbolString = many1 $ letter <|> oneOf ['+', '-', '*', '/', '\'', '<', '>', '=', '!', '%', '&', '.', ':', '?', '@', '$', '^']

symbolP :: Parsec String u Expression
symbolP = Atom . AtomSymbol . Symbol <$> parseSymbolString

atomP :: Parsec String u Expression
atomP = withWhitespace (numberP <|> stringP <|> listP <|> symbolP <?> "wtf value is this dude?")

sExpressionP = withWhitespace $ do
  char '('
  whitespace
  op <- parseSymbolString
  whitespace
  arg <- (atomP <|> sExpressionP) `sepBy` whitespace
  whitespace
  char ')'
  return $ SymbolExpression (Symbol op) arg

expressionP = withWhitespace $ sExpressionP <|> atomP

multipleExpressionsP = withWhitespace $ expressionP `sepBy` whitespace

tokenize = withParseErr . parse multipleExpressionsP "LithParserError"

--
--
