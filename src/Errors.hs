module Errors where

import Text.Parsec

data EvalError
  = EvalError String
  | ParserError ParseError
  deriving (Show, Eq)

withParseErr (Left e) = Left $ ParserError e
withParseErr (Right x) = Right x
