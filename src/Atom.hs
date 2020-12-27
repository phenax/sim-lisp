{-# LANGUAGE LambdaCase #-}

module Atom where

import Errors

data Expression
  = Atom Atom
  | SymbolExpression [Expression]
  deriving (Eq, Show)

data Atom
  = AtomString String
  | AtomInt Integer
  | AtomBool Bool
  | AtomList [Expression]
  | AtomSymbol String
  | AtomLambda [String] Expression
  deriving (Eq)

instance Show Atom where
  show (AtomInt n) = show n
  show (AtomString s) = "\"" ++ s ++ "\""
  show (AtomList exprs) = "(" ++ (unwords . map show $ exprs) ++ ")"
  show (AtomSymbol s) = s
  show (AtomBool b) = if b then "T" else "F"
  show (AtomLambda props _expr) = "<lambda:(" ++ show props ++ ")>"

letPair :: Expression -> Either Error (String, Expression)
letPair = \case
  SymbolExpression [Atom (AtomSymbol s), expr] -> Right (s, expr)
  _ -> Left $ EvalError "Invalid `let` binding"

isSymbol :: Expression -> Bool
isSymbol = \case
  Atom (AtomSymbol _) -> True
  _ -> False

toSymbolString :: Expression -> Maybe String
toSymbolString = \case
  Atom (AtomSymbol s) -> Just s
  _ -> Nothing

mapInt :: (Integer -> Integer) -> Atom -> Atom
mapInt fn = \case
  AtomInt x -> AtomInt $ fn x
  x -> x
