module Errors where

import Text.Parsec

-- TODO: Better error handling
data Error
  = EvalError String
  | ParserError ParseError
  deriving (Show, Eq)

withParseErr (Left e) = Left $ ParserError e
withParseErr (Right x) = Right x
