{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Eval where

import Atom
import Control.Applicative
import Control.Monad
import qualified Data.ByteString.Char8 as BChar8
import Data.FileEmbed
import Data.List
import qualified Data.Map as Map
import Debug.Trace
import Errors
import LParser
import Utils

type Scope = Map.Map String Atom

type MacroEvaluator = Scope -> [Expression] -> Either Error (Atom, Scope)

stdlibContent :: String
stdlibContent =
  unwords $
    map
      BChar8.unpack
      [ $(embedFile "./src/stdlib/core.sim"),
        $(embedFile "./src/stdlib/list.sim")
      ]

builtins =
  [ ("+", foldIntsE (+) 0),
    ("-", foldIntsE (-) 0),
    ("*", foldIntsE (*) 1),
    ("/", foldIntsE div 1),
    ("=", compare2E [EQ]),
    ("<", compare2E [LT]),
    ("<=", compare2E [LT, EQ]),
    (">", compare2E [GT]),
    (">=", compare2E [GT, EQ]),
    ("quote", quoteE),
    ("eval", evalE),
    ("lambda", lambdaE),
    ("cons", consE),
    ("car", carE),
    ("cdr", cdrE),
    ("if", ifE),
    ("do", doblockE),
    ("declare", declareE),
    ("def", defineFunctionE),
    ("let", letbindingE),
    ("import", importE)
  ]

emptyScope = Map.empty

evalConcat :: Scope -> [Expression] -> Either Error [(Atom, Scope)]
evalConcat scope = mergeM . map (evalExpression scope)

foldIntsE :: (Integer -> Integer -> Integer) -> Integer -> Scope -> [Expression] -> Either Error (Atom, Scope)
foldIntsE fn init scope = folder scope <=< evalConcat scope
  where
    folder :: Scope -> [(Atom, Scope)] -> Either Error (Atom, Scope)
    folder scope = \case
      [] -> Right (AtomInt init, scope)
      [(AtomInt a, _)] -> Right (AtomInt a, scope)
      ((AtomInt a, currentScope) : tail) -> (,scope) . mapInt (`fn` a) . fst <$> folder currentScope tail
      _ -> Left $ EvalError "Invalid set of params"

lambdaE :: MacroEvaluator
lambdaE scope = \case
  [SymbolExpression args, body] ->
    if all isSymbol args
      then (\params -> (AtomLambda params body, scope)) <$> (toEither . mergeM . map toSymbolString) args
      else Left $ EvalError "Invalid arguments passed to `lambda` expression"
  _ -> Left $ EvalError "Invalid `lambda` expression"

doblockE :: MacroEvaluator
doblockE scope = foldl evaluateExpr (Right (AtomInt 0, scope))
  where
    evaluateExpr = \result expr -> do
      (_, lastScope) <- result
      evalExpression lastScope expr

declareE :: MacroEvaluator
declareE scope = \case
  [Atom (AtomSymbol (Atom (AtomLabel k))), expr] ->
    (\value -> (value, Map.insert k value scope)) <$> evalExpressionPure scope expr
  _ -> Left $ EvalError "Invalid `declare` expression"

defineFunctionE :: MacroEvaluator
defineFunctionE scope = \case
  [Atom (AtomSymbol (Atom (AtomLabel name))), SymbolExpression args, body] ->
    if all isSymbol args
      then defineLambda <$> (toEither . mergeM . map toSymbolString) args
      else Left $ EvalError "Invalid arguments passed to `lambda` expression"
    where
      defineLambda = \params -> let lambda = AtomLambda params body in (lambda, Map.insert name lambda scope)
  _ -> Left $ EvalError "Invalid `def` expression"

letbindingE :: MacroEvaluator
letbindingE scope = \case
  [SymbolExpression params, expression] -> do
    -- Evaluate params
    paramMap <-
      let bindingsToScope = fmap (Map.fromList . map (mapSnd fst)) . flattenPairBySnd
          evalArgs = map $ fmap (mapSnd (evalExpression scope)) . letPair
       in bindingsToScope <=< (mergeM . evalArgs) $ params
    -- Inject params into scope
    -- Evaluate body with newScope
    let newScope = paramMap `Map.union` scope
     in evalExpression newScope expression
  _ -> Left $ EvalError "Invalid `let` expression"

-- TODO: Implement after io setup
importE :: MacroEvaluator
importE scope = \case
  [Atom (AtomString file)] -> Left $ EvalError "TODO: Import impl"
  _ -> Left $ EvalError "Invalid import expression"

ifE :: MacroEvaluator
ifE scope = \case
  [exp1, thenBody, elseBody] ->
    let isTruthy = \case
          AtomBool x -> x
          AtomNil -> False
          _ -> True
     in do
          condAtom <- evalExpressionPure scope exp1
          result <- evalExpressionPure scope (if isTruthy condAtom then thenBody else elseBody)
          return (result, scope)
  _ -> Left $ EvalError "Invalid number of arguments"

compare2E :: [Ordering] -> MacroEvaluator
compare2E ordering scope = \case
  [exp1, exp2] -> do
    a <- evalExpressionPure scope exp1
    b <- evalExpressionPure scope exp2
    return (AtomBool (check a b), scope)
    where
      check = \a b -> compareAtom a b `elem` ordering
  _ -> Left $ EvalError "Invalid number of arguments"

quoteE :: MacroEvaluator
quoteE scope = \case
  [SymbolExpression []] -> Right (AtomNil, scope)
  [expr] -> Right (AtomSymbol expr, scope)
  _ -> Left $ EvalError "Invalid number of arguments to `quote`"

evalE :: MacroEvaluator
evalE scope = \case
  [expr] -> do
    expr <- evalExpressionPure scope expr
    case expr of
      AtomSymbol expr -> evalExpression scope expr
      _ -> Left $ EvalError $ "Invalid argument passed to `eval`"
  _ -> Left $ EvalError "Invalid number of arguments to `eval`"

carE :: MacroEvaluator
carE scope = \case
  [expr] -> do
    expr <- evalExpressionPure scope expr
    case expr of
      AtomNil -> Right (AtomNil, scope)
      AtomSymbol (SymbolExpression []) -> Right (AtomNil, scope)
      AtomSymbol (SymbolExpression (h : _lst)) -> evalExpression scope h
      _ -> Left $ EvalError "Invalid argument passed to `car`"
  _ -> Left $ EvalError "Invalid number of arguments passed to `car`"

cdrE :: MacroEvaluator
cdrE scope = \case
  [expr] -> do
    expr <- evalExpressionPure scope expr
    case expr of
      AtomNil -> Right (AtomNil, scope)
      AtomSymbol (SymbolExpression []) -> Right (AtomNil, scope)
      AtomSymbol (SymbolExpression [_h]) -> Right (AtomNil, scope)
      AtomSymbol (SymbolExpression (_h : lst)) -> Right (AtomSymbol $ SymbolExpression lst, scope)
      _ -> Left $ EvalError "Invalid argument passed to `cdr`"
  _ -> Left $ EvalError "Invalid number of arguments passed to `cdr`"

consE :: MacroEvaluator
consE scope = \case
  [expr1, expr2] -> do
    a <- evalExpressionPure scope expr1
    b <- evalExpressionPure scope expr2
    case b of
      AtomNil -> Right $ (AtomSymbol $ SymbolExpression [Atom a], scope)
      AtomSymbol (SymbolExpression ls) -> Right $ (AtomSymbol $ SymbolExpression $ Atom a : ls, scope)
      _ -> Left $ EvalError "Invalid argument passed to `cons`"
  _ -> Left $ EvalError "Invalid number of arguments passed to `cons`"

applyLambda :: [String] -> Expression -> MacroEvaluator
applyLambda params body scope arguments = do
  newScope <- toScope params
  result <- evalExpressionPure newScope body
  return (result, scope)
  where
    toScope params = (`Map.union` scope) . Map.fromList . zip params . map fst <$> evalConcat scope arguments

applyAsSymbol :: String -> Scope -> [Expression] -> Maybe (Either Error (Atom, Scope))
applyAsSymbol fn scope arguments = evaluateValue <$> Map.lookup fn scope
  where
    evaluateValue = \case
      AtomLambda params body -> applyLambda params body scope arguments
      _ -> Left $ EvalError ("Invalid call. `" ++ fn ++ "` is not a macro")

applyBuiltin :: String -> Scope -> [Expression] -> Maybe (Either Error (Atom, Scope))
applyBuiltin fn scope arguments =
  (\fn -> fn scope arguments) . snd <$> find ((==) fn . fst) builtins

applyE :: String -> MacroEvaluator
applyE fn scope arguments = flatten $ applyAsSymbol fn scope arguments <|> applyBuiltin fn scope arguments
  where
    flatten = \case
      Just e -> e
      Nothing -> Left $ EvalError ("Not found boeey:: " ++ fn)

-- Evaluate expression without leaking scope
evalExpressionPure :: Scope -> Expression -> Either Error Atom
evalExpressionPure scope = fmap fst . evalExpression scope

-- Evaluate expression with leaked scope
evalExpression :: Scope -> Expression -> Either Error (Atom, Scope)
evalExpression scope = \case
  Atom atom -> case atom of
    AtomSymbol (Atom (AtomLabel k)) -> case Map.lookup k scope of
      Just value -> Right (value, scope)
      Nothing -> Left $ EvalError $ "Variable " ++ k ++ " not found in scope"
    a -> Right (a, scope)
  SymbolExpression (operation : lst) -> case operation of
    -- TODO: predefined function as symbol
    Atom (AtomSymbol (Atom (AtomLabel opSymbol))) -> applyE opSymbol scope lst
    SymbolExpression exprs -> do
      atom <- evalExpressionPure scope (SymbolExpression exprs)
      case atom of
        AtomLambda params body -> applyLambda params body scope lst
        _ -> Left $ EvalError "Invalid syntax"
    _ -> Left $ EvalError "TODO: Not impl 1"
  _ -> Left $ EvalError "TODO: Not impl out"

evaluateWithScope :: Scope -> [Expression] -> Either Error (Atom, Scope)
evaluateWithScope scope = evalExpression scope . SymbolExpression . (createLabel "do" :)

loadLibrarysIntoScope :: Scope -> Either Error Scope
loadLibrarysIntoScope scope = snd <$> (tokenize stdlibContent >>= evaluateWithScope scope)

evaluateWithStdlib :: Scope -> [Expression] -> Either Error (Atom, Scope)
evaluateWithStdlib parentScope exprs = do
  scope <- loadLibrarysIntoScope parentScope
  evaluateWithScope scope exprs

evaluate :: [Expression] -> Either Error Atom
evaluate exprs = fst <$> evaluateWithStdlib emptyScope exprs

interpret :: Scope -> String -> Either Error (Atom, Scope)
interpret scope = evaluateWithStdlib scope <=< tokenize

--
