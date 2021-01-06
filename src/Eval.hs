{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Eval where

import Atom
import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Except
import qualified Data.ByteString.Char8 as BChar8
import Data.FileEmbed
import Data.List
import qualified Data.Map as Map
import qualified Effects.ConsoleIO as ConsoleIO
import Errors
import LParser
import Utils

type Scope = Map.Map String Atom

emptyScope = Map.empty

type EvalResultPure = ExceptWithEvalError Atom

type EvalResult = ExceptWithEvalError (Atom, Scope)

type Evaluator = Scope -> [Expression] -> EvalResult

stdlibContent :: String
stdlibContent =
  unwords $
    map
      BChar8.unpack
      [ $(embedFile "./src/stdlib/core.sim"),
        $(embedFile "./src/stdlib/list.sim")
      ]

builtins :: [(String, Evaluator)]
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
    ("number?", isNumberE),
    ("boolean?", isBooleanE),
    ("display", displayE),
    ("import", importE)
  ]

displayE :: Evaluator
displayE scope exprs = do
  result <- foldl monadConcat (pure []) . map (fmap show . evalExpressionPure scope) $ exprs
  liftExceptT . ConsoleIO.putStrLn . unwords $ result
  return (AtomNil, scope)

typeCheck :: (Atom -> Bool) -> Evaluator
typeCheck check scope = \case
  (head : _) -> do
    result <- evalExpressionPure scope head
    return (AtomBool (check result), scope)
  _ -> pure (AtomBool False, scope)

isBooleanE = typeCheck $ \case
  AtomBool _ -> True
  AtomNil -> True
  _ -> False

isNumberE = typeCheck $ \case
  AtomInt _ -> True
  _ -> False

evalConcat :: Scope -> [Expression] -> ExceptWithEvalError [(Atom, Scope)]
evalConcat scope = mergeM . map (evalExpression scope)

foldIntsE :: (Integer -> Integer -> Integer) -> Integer -> Evaluator
foldIntsE fn init scope expr = evalConcat scope expr >>= folder scope
  where
    folder :: Scope -> [(Atom, Scope)] -> ExceptWithEvalError (Atom, Scope)
    folder scope = \case
      [] -> return (AtomInt init, scope)
      [(AtomInt a, _)] -> return (AtomInt a, scope)
      ((AtomInt a, currentScope) : tail) -> (,scope) . mapInt (`fn` a) . fst <$> folder currentScope tail
      _ -> withErr $ EvalError "Invalid set of params"

lambdaE :: Evaluator
lambdaE scope = \case
  [SymbolExpression args, body] ->
    if all isSymbol args
      then (\params -> (AtomLambda params body, scope)) <$> (toEither . mergeM . map toSymbolString) args
      else withErr $ EvalError "Invalid arguments passed to `lambda` expression"
  _ -> withErr $ EvalError "Invalid `lambda` expression"

doblockE :: Evaluator
doblockE scope = foldl evaluateExpr (pure (AtomInt 0, scope))
  where
    evaluateExpr :: EvalResult -> Expression -> EvalResult
    evaluateExpr result expr = result >>= (`evalExpression` expr) . snd

declareE :: Evaluator
declareE scope = \case
  [Atom (AtomSymbol (Atom (AtomLabel k))), expr] ->
    (\value -> (value, Map.insert k value scope)) <$> evalExpressionPure scope expr
  _ -> withErr $ EvalError "Invalid `declare` expression"

defineFunctionE :: Evaluator
defineFunctionE scope = \case
  [Atom (AtomSymbol (Atom (AtomLabel name))), SymbolExpression args, body] ->
    if all isSymbol args
      then fmap defineLambda . toEither . mergeM . map toSymbolString $ args
      else withErr $ EvalError "Invalid arguments passed to `lambda` expression"
    where
      defineLambda params = let lambda = AtomLambda params body in (lambda, Map.insert name lambda scope)
  _ -> withErr $ EvalError "Invalid `def` expression"

letbindingE :: Evaluator
letbindingE scope = \case
  [SymbolExpression params, expression] -> do
    paramMap <-
      let bindingsToScope = fmap (Map.fromList . map (mapSnd fst)) . flattenPairBySnd
          evalArgs = map $ fmap (mapSnd (evalExpression scope)) . letPair
       in mergeM (evalArgs params) >>= bindingsToScope
    let newScope = paramMap `Map.union` scope
     in evalExpression newScope expression
  _ -> withErr $ EvalError "Invalid `let` expression"

-- TODO: Implement after io setup
importE :: Evaluator
importE scope = \case
  [Atom (AtomString file)] -> withErr $ EvalError "TODO: Import impl"
  _ -> withErr $ EvalError "Invalid import expression"

ifE :: Evaluator
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
  _ -> withErr $ EvalError "Invalid number of arguments"

compare2E :: [Ordering] -> Evaluator
compare2E ordering scope = \case
  [exp1, exp2] -> do
    a <- evalExpressionPure scope exp1
    b <- evalExpressionPure scope exp2
    return (AtomBool (check a b), scope)
    where
      check = \a b -> compareAtom a b `elem` ordering
  _ -> withErr $ EvalError "Invalid number of arguments"

quoteE :: Evaluator
quoteE scope = \case
  [SymbolExpression []] -> pure (AtomNil, scope)
  [expr] -> pure (AtomSymbol expr, scope)
  _ -> withErr $ EvalError "Invalid number of arguments to `quote`"

evalE :: Evaluator
evalE scope = \case
  [expr] -> do
    expr <- evalExpressionPure scope expr
    case expr of
      AtomSymbol expr -> evalExpression scope expr
      _ -> withErr $ EvalError $ "Invalid argument passed to `eval`"
  _ -> withErr $ EvalError "Invalid number of arguments to `eval`"

carE :: Evaluator
carE scope = \case
  [expr] -> do
    expr <- evalExpressionPure scope expr
    case expr of
      AtomNil -> pure (AtomNil, scope)
      AtomSymbol (SymbolExpression []) -> pure (AtomNil, scope)
      AtomSymbol (SymbolExpression (h : _lst)) -> evalExpression scope h
      _ -> withErr $ EvalError "Invalid argument passed to `car`"
  _ -> withErr $ EvalError "Invalid number of arguments passed to `car`"

cdrE :: Evaluator
cdrE scope = \case
  [expr] -> do
    expr <- evalExpressionPure scope expr
    case expr of
      AtomNil -> pure (AtomNil, scope)
      AtomSymbol (SymbolExpression []) -> pure (AtomNil, scope)
      AtomSymbol (SymbolExpression [_h]) -> pure (AtomNil, scope)
      AtomSymbol (SymbolExpression (_h : lst)) -> pure (AtomSymbol $ SymbolExpression lst, scope)
      _ -> withErr $ EvalError "Invalid argument passed to `cdr`"
  _ -> withErr $ EvalError "Invalid number of arguments passed to `cdr`"

consE :: Evaluator
consE scope = \case
  [expr1, expr2] -> do
    a <- evalExpressionPure scope expr1
    b <- evalExpressionPure scope expr2
    case b of
      AtomNil -> pure (AtomSymbol $ SymbolExpression [Atom a], scope)
      AtomSymbol (SymbolExpression ls) -> pure (AtomSymbol $ SymbolExpression $ Atom a : ls, scope)
      _ -> withErr $ EvalError "Invalid argument passed to `cons`"
  _ -> withErr $ EvalError "Invalid number of arguments passed to `cons`"

applyLambda :: [String] -> Expression -> Evaluator
applyLambda params body scope arguments =
  let toScope params = (`Map.union` scope) . Map.fromList . zip params . map fst <$> evalConcat scope arguments
   in do
        newScope <- toScope params
        result <- evalExpressionPure newScope body
        return (result, scope)

applyAsSymbol :: String -> Scope -> [Expression] -> Maybe (ExceptWithEvalError (Atom, Scope))
applyAsSymbol fn scope arguments = evaluateValue <$> Map.lookup fn scope
  where
    evaluateValue = \case
      AtomLambda params body -> applyLambda params body scope arguments
      _ -> withErr $ EvalError ("Invalid call. `" ++ fn ++ "` is not a function")

applyBuiltin :: String -> Scope -> [Expression] -> Maybe (ExceptWithEvalError (Atom, Scope))
applyBuiltin fn scope arguments =
  (\fn -> fn scope arguments) . snd <$> find ((==) fn . fst) builtins

applyE :: String -> Evaluator
applyE fn scope arguments =
  flatten $ applyAsSymbol fn scope arguments <|> applyBuiltin fn scope arguments
  where
    flatten = \case
      Just e -> e
      Nothing -> withErr $ EvalError ("Not found boeey:: " ++ fn)

-- Evaluate expression without leaking scope
evalExpressionPure :: Scope -> Expression -> EvalResultPure
evalExpressionPure scope = fmap fst . evalExpression scope

-- Evaluate expression with leaked scope
evalExpression :: Scope -> Expression -> EvalResult
evalExpression scope = \case
  Atom atom -> case atom of
    AtomSymbol (Atom (AtomLabel k)) -> case Map.lookup k scope of
      Just value -> pure (value, scope)
      Nothing -> withErr $ EvalError $ "Variable " ++ k ++ " not found in scope"
    a -> pure (a, scope)
  SymbolExpression (operation : lst) -> case operation of
    -- TODO: predefined function as symbol
    Atom (AtomSymbol (Atom (AtomLabel opSymbol))) -> applyE opSymbol scope lst
    SymbolExpression exprs -> do
      atom <- evalExpressionPure scope (SymbolExpression exprs)
      case atom of
        AtomLambda params body -> applyLambda params body scope lst
        _ -> withErr $ EvalError "Invalid syntax"
    _ -> withErr $ EvalError "TODO: Not impl 1"
  _ -> withErr $ EvalError "TODO: Not impl out"

evaluateWithScope :: Scope -> [Expression] -> EvalResult
evaluateWithScope scope = evalExpression scope . SymbolExpression . (createLabel "do" :)

loadLibraryIntoScope :: Scope -> ExceptWithEvalError Scope
loadLibraryIntoScope scope = fmap snd $ except (tokenize stdlibContent) >>= evaluateWithScope scope

evaluateWithStdlib :: Scope -> [Expression] -> EvalResult
evaluateWithStdlib parentScope exprs = do
  scope <- loadLibraryIntoScope parentScope
  evaluateWithScope scope exprs

evaluate :: [Expression] -> EvalResultPure
evaluate exprs = fst <$> evaluateWithStdlib emptyScope exprs

interpret :: Scope -> String -> EvalResult
interpret scope = evaluateWithStdlib scope <=< except . tokenize

--
