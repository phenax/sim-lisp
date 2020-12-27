{-# LANGUAGE LambdaCase #-}

module Atom where

import Errors

data Expression
  = Atom Atom
  | SymbolExpression [Expression]
  deriving (Eq)

instance Show Expression where
  show (Atom (AtomInt n)) = show n
  show (Atom (AtomString s)) = "\"" ++ s ++ "\""
  show (Atom (AtomList exprs)) = "(" ++ (unwords . map show $ exprs) ++ ")"
  show (Atom (AtomSymbol s)) = s
  show (SymbolExpression lst) = "(" ++ (unwords . map show $ lst) ++ ")"

data Atom
  = AtomString String
  | AtomInt Integer
  | AtomBool Bool
  | AtomList [Expression]
  | AtomSymbol String
  | AtomLambda [String] Expression
  deriving (Show, Eq)

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
