{-# LANGUAGE LambdaCase #-}

module Atom where

import Control.Monad.Trans.Except
import qualified Data.Map as Map
import Errors

type Scope = Map.Map String Atom

type CallStack = [Scope]

emptyScope = Map.empty

emptyCallStack = [emptyScope]

findDefinition :: String -> CallStack -> Maybe Atom
findDefinition key [] = Nothing
findDefinition key (scope : stack) = case Map.lookup key scope of
  Just result -> Just result
  Nothing -> findDefinition key stack

pushToStack :: CallStack -> Scope -> CallStack
pushToStack cs s = s : cs

defineInScope :: String -> Atom -> CallStack -> CallStack
defineInScope key value = \case
  [] -> [Map.insert key value emptyScope]
  [scope] -> [Map.insert key value scope]
  (scope : stack) -> Map.insert key value scope : stack

mergeCallStack :: CallStack -> CallStack -> CallStack
mergeCallStack [] [] = []
mergeCallStack x [] = x
mergeCallStack [] x = x
mergeCallStack closure calling =
  mergeCallStack (init closure) (init calling) ++ [Map.union (last calling) (last closure)]

data Expression
  = Atom Atom
  | SymbolExpression [Expression]
  deriving (Eq)

instance Show Expression where
  show (SymbolExpression exprs) = "'(" ++ (unwords . map show) exprs ++ ")"
  show (Atom a) = show a

data Atom
  = AtomNil
  | AtomInt Integer
  | AtomBool Bool
  | AtomLabel String
  | AtomSymbol Expression
  | AtomLambda CallStack [String] Expression
  | AtomSyntax String [(Expression, Expression)]
  | AtomString String
  deriving (Eq)

instance Ord Atom where
  compare (AtomInt a) (AtomInt b) = compare a b
  compare (AtomString a) (AtomString b) = compare a b
  compare (AtomBool a) (AtomBool b) = compare a b
  compare AtomNil AtomNil = EQ
  compare _ _ = LT

instance Show Atom where
  show (AtomInt n) = show n
  show AtomNil = "Nil"
  show (AtomString s) = s
  show (AtomSymbol s) = show s
  show (AtomLabel s) = s
  show (AtomBool b) = if b then "T" else "F"
  show (AtomLambda _ props _expr) = "<lambda:(" ++ show props ++ ")>"
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
