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

compareAtom (AtomInt a) (AtomInt b) = compare a b
compareAtom (AtomString a) (AtomString b) = compare a b
compareAtom (AtomBool a) (AtomBool b) = compare a b
compareAtom (AtomList a) (AtomList b) = compare (length a) (length b)
compareAtom _ _ = LT

--compare (AtomString s) = "\"" ++ s ++ "\""
--compare (AtomList exprs) = "(" ++ (unwords . map show $ exprs) ++ ")"
--compare (AtomSymbol s) = s
--compare (AtomBool b) = if b then "T" else "F"
--compare (AtomLambda props _expr) = "<lambda:(" ++ show props ++ ")>"

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

mapBool :: (Bool -> Bool) -> Atom -> Atom
mapBool fn = \case
  AtomBool x -> AtomBool $ fn x
  x -> x

onlyBool :: Atom -> Either Error Atom
onlyBool = \case
  AtomBool b -> Right $ AtomBool b
  _ -> Left $ EvalError "Invalid data type. Expected boolean"

--
--
--