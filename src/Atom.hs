{-# LANGUAGE LambdaCase #-}

module Atom where

import Control.Monad.Trans.Except
import Data.List
import qualified Data.Map as Map
import Debug.Trace
import Effects
import Errors
import System.Random

type Scope = Map.Map String Atom

type CallStack = [Scope]

type ScopeId = Integer

scopeIdKey = "#('scope')"

newScopeId :: ExceptWithEvalError ScopeId
newScopeId = liftExceptT (randomRIO (0, 999999) :: IO ScopeId)

scopeFromPairs :: ScopeId -> [(String, Atom)] -> Scope
scopeFromPairs sid = Map.fromList . ((scopeIdKey, AtomInt sid) :)

emptyScope :: ScopeId -> Scope
emptyScope sid = scopeFromPairs sid []

mergeScope :: Scope -> Scope -> Scope
mergeScope = Map.union

emptyCallStack :: ScopeId -> CallStack
emptyCallStack sid = [emptyScope sid]

findDefinition :: String -> CallStack -> Maybe Atom
findDefinition key [] = Nothing
findDefinition key (scope : stack) = case Map.lookup key scope of
  Just result -> Just result
  Nothing -> findDefinition key stack

pushToStack :: CallStack -> Scope -> CallStack
pushToStack cs s = s : cs

defineInScope :: String -> Atom -> CallStack -> CallStack
defineInScope key value = \case
  [] -> [Map.insert key value (emptyScope 0)]
  (scope : stack) -> Map.insert key value scope : stack

mapFst fn (a, b) = (fn a, b)

-- TODO: The name conflict problem is here somewhere
-- Find common parent
-- Merge everything above (before the scope in callstack) common parent
-- Keep closure scope for everything after
mergeCallStack :: CallStack -> CallStack -> CallStack
mergeCallStack [] [] = []
mergeCallStack x [] = x
mergeCallStack [] x = x
mergeCallStack closure calling =
  let lastId = Map.lookup scopeIdKey . last
      splitAtDivergence [] [] = Nothing
      splitAtDivergence x [] = Just ([], (x, []))
      splitAtDivergence [] x = Just ([], ([], x))
      splitAtDivergence c1 c2 = do
        c1id <- lastId c1
        c2id <- lastId c2
        if c1id == c2id
          then mapFst (\ls -> ls ++ [mergeScope (last c1) (last c2)]) <$> splitAtDivergence (init c1) (init c2)
          else Just ([], (c1, c2))
   in trace ("CLOSURE: " ++ showCallStack closure) . maybe closure (\(ls, (cl, _)) -> ls ++ cl) $ splitAtDivergence closure calling

--in mergeCallStack (init closure) (init calling) ++ [mergeScope (last calling) (last closure)]

showCallStack :: CallStack -> String
showCallStack = intercalate "\n" . ("STACK" :) . map printScope
  where
    printScope = ('\t' :) . intercalate "; " . map (\(k, v) -> "" ++ k ++ ": " ++ show v) . Map.toList

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

createLabel = Atom . AtomSymbol . Atom . AtomLabel

--
