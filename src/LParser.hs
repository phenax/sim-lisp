{-# LANGUAGE LambdaCase #-}

module LParser where

import Atom
import Errors
import Text.Parsec

whitespace :: Parsec String u String
whitespace = many $ oneOf [' ', '\n', '\t']

commentString :: Parsec String u String
commentString = whitespace >> char ';' >> anyChar `manyTill` newline

-- TODO: Ignore comments
-- ignorable :: Parsec String u String
-- ignorable = commentString <|> whitespace

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

symbolP :: Parsec String u Expression
symbolP = createLabel <$> parseSymbolString
  where
    parseSymbolString =
      many1 $ alphaNum <|> oneOf ['+', '-', '*', '/', '<', '>', '=', '!', '%', '&', '.', '?', '@', '$', '_']

booleanP :: Parsec String u Expression
booleanP = Atom . AtomBool <$> ((True <$ string "T") <|> (False <$ string "F"))

nilP :: Parsec String u Expression
nilP = Atom AtomNil <$ string "Nil"

quotedP :: Parsec String u Expression
quotedP = toExpr <$> (parsePrefix >> listP)
  where
    parsePrefix = char '\''
    toExpr = \case
      SymbolExpression [] -> Atom AtomNil
      expr -> Atom $ AtomSymbol expr

atomP :: Parsec String u Expression
atomP = withWhitespace (numberP <|> stringP <|> nilP <|> booleanP <|> symbolP <|> quotedP <?> "Syntax parsing error")

commentP :: Parsec String u Expression
commentP = Atom AtomNil <$ (char ';' >> anyChar `manyTill` newline)

listP :: Parsec String u Expression
listP = withWhitespace $ do
  char '('
  whitespace
  arg <- (atomP <|> listP) `sepBy` whitespace
  whitespace
  char ')'
  return $ SymbolExpression arg

expressionP = withWhitespace $ listP <|> atomP <|> commentP

multipleExpressionsP = withWhitespace $ expressionP `sepBy` whitespace

tokenize = withParseErr . parse multipleExpressionsP "ParserError"

--
