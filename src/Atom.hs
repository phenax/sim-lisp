{-# LANGUAGE LambdaCase #-}

module Atom where

import Control.Monad.Trans.Except
import Errors

data Expression
  = Atom Atom
  | SymbolExpression [Expression]
  deriving (Eq, Show)

data Atom
  = AtomNil
  | AtomInt Integer
  | AtomBool Bool
  | AtomLabel String
  | AtomSymbol Expression
  | AtomLambda [String] Expression
  | AtomSyntax String [(Expression, Expression)]
  | AtomString String
  deriving (Eq)

compareAtom (AtomInt a) (AtomInt b) = compare a b
compareAtom (AtomString a) (AtomString b) = compare a b
compareAtom (AtomBool a) (AtomBool b) = compare a b
compareAtom AtomNil AtomNil = EQ
compareAtom _ _ = LT

--compare (AtomString s) = "\"" ++ s ++ "\""
--compare (AtomList exprs) = "(" ++ (unwords . map show $ exprs) ++ ")"
--compare (AtomSymbol s) = s
--compare (AtomBool b) = if b then "T" else "F"
--compare (AtomLambda props _expr) = "<lambda:(" ++ show props ++ ")>"

instance Show Atom where
  show (AtomInt n) = show n
  show AtomNil = "Nil"
  show (AtomString s) = s
  show (AtomSymbol s) = "<symbol:" ++ show s ++ ">"
  show (AtomLabel s) = s
  show (AtomBool b) = if b then "T" else "F"
  show (AtomLambda props _expr) = "<lambda:(" ++ show props ++ ")>"
  show (AtomSyntax name _) = "<syntax:(" ++ name ++ " ...)>"

type ExceptWithEvalError = ExceptT Error IO

liftExceptT :: IO a -> ExceptT e IO a
liftExceptT = ExceptT . fmap Right

withErr = except . Left

letPair :: Expression -> ExceptWithEvalError (String, Expression)
letPair = \case
  SymbolExpression [Atom (AtomSymbol sym), expr] -> case sym of
    Atom (AtomLabel label) -> pure (label, expr)
    _ -> withErr $ EvalError "Invalid `let` binding"
  _ -> withErr $ EvalError "Invalid `let` binding"

isSymbol :: Expression -> Bool
isSymbol = \case
  Atom (AtomSymbol _) -> True
  _ -> False

toSymbolString :: Expression -> Maybe String
toSymbolString = \case
  Atom (AtomSymbol (Atom (AtomLabel s))) -> Just s
  _ -> Nothing

mapInt :: (Integer -> Integer) -> Atom -> Atom
mapInt fn = \case
  AtomInt x -> AtomInt $ fn x
  x -> x

mapBool :: (Bool -> Bool) -> Atom -> Atom
mapBool fn = \case
  AtomBool x -> AtomBool $ fn x
  x -> x

onlyBool :: Atom -> ExceptWithEvalError Atom
onlyBool = \case
  AtomBool b -> pure $ AtomBool b
  _ -> withErr $ EvalError "Invalid data type. Expected boolean"

createLabel = Atom . AtomSymbol . Atom . AtomLabel

--
