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
import Debug.Trace
import qualified Effects as Eff
import Errors
import LParser
import Utils

type EvalResultPure = ExceptWithEvalError Atom

type EvalResult = ExceptWithEvalError (Atom, CallStack)

type Evaluator = CallStack -> [Expression] -> EvalResult

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
  [ -- Primitive helpers
    ("add", intBinaryOpt (+) id),
    ("mul", intBinaryOpt (*) id),
    ("sub", intBinaryOpt (-) (* (-1))),
    ("div", intBinaryOpt div id),
    ("mod", intBinaryOpt mod id),
    ("eq?", compare2E [EQ]),
    ("lt?", compare2E [LT]),
    ("lte?", compare2E [LT, EQ]),
    ("gt?", compare2E [GT]),
    ("gte?", compare2E [GT, EQ]),
    -- Expression manipulation
    ("quote", quoteE),
    ("eval", evalE),
    ("apply", applyExpressionE),
    ("cons", consE),
    ("car", carE),
    ("cdr", cdrE),
    -- Definition
    ("let", letbindingE),
    ("lambda", lambdaE),
    ("def", defineE),
    ("declare", defineE),
    -- Syntax helpers
    ("if", ifE),
    ("do", doblockE),
    -- Check type
    ("number?", isNumberE),
    ("boolean?", isBooleanE),
    -- i/o
    ("display", displayE),
    ("import", importE)
  ]

displayE :: Evaluator
displayE callstack exprs = do
  result <- concatM . map (fmap show . evalExpressionPure callstack) $ exprs
  liftExceptT . Eff.putStrLn . intercalate "" $ result
  return (AtomNil, callstack)

typeCheck :: (Atom -> Bool) -> Evaluator
typeCheck check callstack = \case
  (head : _) -> do
    value <- evalExpressionPure callstack head
    return (AtomBool (check value), callstack)
  _ -> pure (AtomBool False, callstack)

isBooleanE = typeCheck $ \case
  AtomBool _ -> True
  AtomNil -> True
  _ -> False

isNumberE = typeCheck $ \case
  AtomInt _ -> True
  _ -> False

evalConcat :: CallStack -> [Expression] -> ExceptWithEvalError [(Atom, CallStack)]
evalConcat callstack = rconcatM . map (evalExpression callstack)

intBinaryOpt :: (Integer -> Integer -> Integer) -> (Integer -> Integer) -> Evaluator
intBinaryOpt binaryOp unaryOp callstack expr = evalConcat callstack expr >>= runOp . map fst
  where
    runOp :: [Atom] -> ExceptWithEvalError (Atom, CallStack)
    runOp = \case
      [AtomInt a, AtomInt b] -> pure (AtomInt $ binaryOp b a, callstack)
      [AtomInt a] -> pure (AtomInt $ unaryOp a, callstack)
      _ -> withErr $ EvalError "Invalid set of params"

doblockE :: Evaluator
doblockE callstack = foldl evaluateExpr (pure (AtomInt 0, callstack))
  where
    evaluateExpr :: EvalResult -> Expression -> EvalResult
    evaluateExpr result expr = result >>= (`evalExpression` expr) . snd

toLambdaExpr callstack body args =
  (\params -> (AtomLambda callstack params body, callstack)) <$> symbolsToParams args
  where
    symbolsToParams = fromMaybe . concatM . map toSymbolString

lambdaE :: Evaluator
lambdaE callstack = \case
  [SymbolExpression args, body] ->
    if all isSymbol args
      then toLambdaExpr callstack body args
      else withErr $ EvalError "Invalid arguments passed to `lambda` expression"
  _ -> withErr $ EvalError "Invalid `lambda` expression"

defineE :: Evaluator
defineE callstack = \case
  [Atom (AtomSymbol (Atom (AtomLabel k))), expr] ->
    createDefinition k . (,callstack) <$> evalExpressionPure callstack expr
  [Atom (AtomSymbol (Atom (AtomLabel name))), SymbolExpression args, body] ->
    if all isSymbol args
      then createDefinition name <$> toLambdaExpr callstack body args
      else withErr $ EvalError "Invalid `lambda` expression"
  _ -> withErr $ EvalError "Invalid `def` expression"
  where
    createDefinition name (lambda, callstack) = (lambda, defineInScope name lambda callstack)

resolveScope :: ExceptWithEvalError CallStack -> ExceptWithEvalError (String, Expression) -> ExceptWithEvalError CallStack
resolveScope sc binding = do
  callstack <- sc
  (name, expr) <- binding
  value <- evalExpressionPure callstack expr
  return $ defineInScope name value callstack

letbindingE :: Evaluator
letbindingE callstack = \case
  [SymbolExpression params, expression] -> do
    new <- foldl resolveScope (pure $ pushToStack callstack emptyScope) . map letPair $ params
    evalExpression new expression
  _ -> withErr $ EvalError "Invalid `let` expression"

-- TODO: Implement after io setup
importE :: Evaluator
importE callstack = \case
  [Atom (AtomString file)] -> withErr $ EvalError "TODO: Import impl"
  _ -> withErr $ EvalError "Invalid import expression"

ifE :: Evaluator
ifE callstack = \case
  [exp1, thenBody, elseBody] ->
    do
      condAtom <- evalExpressionPure callstack exp1
      result <- evalExpressionPure callstack (if isTruthy condAtom then thenBody else elseBody)
      return (result, callstack)
    where
      isTruthy = \case
        AtomBool x -> x
        AtomNil -> False
        _ -> True
  _ -> withErr $ EvalError "Invalid number of arguments"

compare2E :: [Ordering] -> Evaluator
compare2E ordering callstack = \case
  [exp1, exp2] -> liftJoin2 compareAtom (eval exp1) (eval exp2)
    where
      eval = evalExpressionPure callstack
      compareAtom a b = return (AtomBool (check a b), callstack)
      check = \a b -> compare a b `elem` ordering
  _ -> withErr $ EvalError "Invalid number of arguments passed for comparison"

quoteE :: Evaluator
quoteE callstack = \case
  [SymbolExpression []] -> pure (AtomNil, callstack)
  [expr] -> pure (AtomSymbol expr, callstack)
  _ -> withErr $ EvalError "Invalid number of arguments to `quote`"

evalE :: Evaluator
evalE callstack = \case
  [expr] ->
    evalExpressionPure callstack expr >>= \case
      AtomSymbol expr -> evalExpression callstack expr
      atom -> pure (atom, callstack)
  _ -> withErr $ EvalError "Invalid number of arguments to `eval`"

carE :: Evaluator
carE callstack = \case
  [expr] ->
    evalExpressionPure callstack expr >>= \case
      AtomNil -> pure (AtomNil, callstack)
      AtomSymbol (SymbolExpression []) -> pure (AtomNil, callstack)
      AtomSymbol (SymbolExpression (h : _lst)) -> evalExpression callstack h
      _ -> withErr $ EvalError "Invalid argument passed to `car`"
  _ -> withErr $ EvalError "Invalid number of arguments passed to `car`"

cdrE :: Evaluator
cdrE callstack = \case
  [expr] ->
    evalExpressionPure callstack expr >>= \case
      AtomNil -> pure (AtomNil, callstack)
      AtomSymbol (SymbolExpression []) -> pure (AtomNil, callstack)
      AtomSymbol (SymbolExpression [_h]) -> pure (AtomNil, callstack)
      AtomSymbol (SymbolExpression (_h : lst)) -> pure (AtomSymbol $ SymbolExpression lst, callstack)
      _ -> withErr $ EvalError "Invalid argument passed to `cdr`"
  _ -> withErr $ EvalError "Invalid number of arguments passed to `cdr`"

consE :: Evaluator
consE callstack = \case
  [expr1, expr2] -> liftJoin2 prependAtoms (eval expr1) (eval expr2)
    where
      eval = evalExpressionPure callstack
      prependAtoms a b = case b of
        AtomNil -> pure (AtomSymbol $ SymbolExpression [Atom a], callstack)
        AtomSymbol (SymbolExpression ls) -> pure (AtomSymbol $ SymbolExpression $ Atom a : ls, callstack)
        _ -> withErr $ EvalError "Invalid argument passed to `cons`"
  _ -> withErr $ EvalError "Invalid number of arguments passed to `cons`"

lambdaClosure :: [String] -> CallStack -> [Expression] -> ExceptWithEvalError CallStack
lambdaClosure params callstack args =
  toClosureScope . map fst <$> concatM (map (evalExpression callstack) args)
  where
    toClosureScope = pushToStack callstack . scopeFromPairs . zipParams params
    zipParams ps argValues = case ps of
      [] -> []
      ["...", restP] -> [(restP, listExpr)]
        where
          toList = AtomSymbol . SymbolExpression . map Atom
          listExpr = if null argValues then AtomNil else toList argValues
      (param : rest) -> (param, head argValues) : zipParams rest (tail argValues)

applyLambda :: [String] -> Expression -> Evaluator
applyLambda params body callstack arguments = do
  newClosure <- lambdaClosure params callstack arguments
  result <- evalExpressionPure newClosure body
  return $ trace ("Trace :: " ++ show params ++ " --- " ++ showCallStack (init newClosure)) (result, callstack)

applyAsSymbol :: String -> CallStack -> [Expression] -> Maybe (ExceptWithEvalError (Atom, CallStack))
applyAsSymbol fn callstack arguments = evaluateValue <$> findDefinition fn callstack
  where
    evaluateValue = \case
      AtomLambda closureStack params body ->
        trace ("Function ::::::: " ++ fn ++ " -- " ++ showCallStack (init callstack)) $
          applyLambda params body (mergeCallStack closureStack callstack) arguments
      _ -> withErr $ EvalError ("Invalid call. `" ++ fn ++ "` is not a function")

applyBuiltin :: String -> CallStack -> [Expression] -> Maybe (ExceptWithEvalError (Atom, CallStack))
applyBuiltin fn callstack arguments =
  (\fn -> fn callstack arguments) . snd <$> find ((==) fn . fst) builtins

fnCallE :: String -> Evaluator
fnCallE fn callstack arguments =
  flatten $ applyAsSymbol fn callstack arguments <|> applyBuiltin fn callstack arguments
  where
    flatten = \case
      Just e -> e
      Nothing -> withErr $ EvalError ("Invalid function call : " ++ fn)

applyExpressionE :: Evaluator
applyExpressionE callstack = \case
  [fn, arguments] -> do
    argType <- evalExpressionPure callstack arguments
    case argType of
      AtomSymbol (SymbolExpression args) -> evalExpression callstack $ SymbolExpression (fn : args)
      _ -> withErr $ EvalError "Invalid args: apply expected a list of arguments"
  ls -> withErr $ EvalError ("Invalid number of arguments passed to apply::" ++ show ls)

-- Evaluate expression without leaking scope
evalExpressionPure :: CallStack -> Expression -> EvalResultPure
evalExpressionPure callstack = fmap fst . evalExpression callstack

-- Evaluate expression with leaked scope
evalExpression :: CallStack -> Expression -> EvalResult
evalExpression callstack = \case
  Atom atom -> case atom of
    AtomSymbol (Atom (AtomLabel k)) -> case findDefinition k callstack of
      Just value -> pure (value, callstack)
      Nothing -> withErr $ EvalError $ "Variable " ++ k ++ " not found in scope"
    a -> pure (a, callstack)
  SymbolExpression exprs ->
    case head exprs of
      -- TODO: builtin as symbol
      Atom (AtomSymbol (Atom (AtomLabel opSymbol))) -> fnCallE opSymbol callstack lst
      SymbolExpression exprs -> symListExprE exprs
      Atom (AtomLambda closureStack params body) -> runLambda closureStack params body
      _ -> withErr $ EvalError ("TODO: Not impl 1 -- " ++ show exprs)
    where
      lst = tail exprs
      runLambda cs params body = applyLambda params body (mergeCallStack cs callstack) lst
      symListExprE exprs =
        evalExpressionPure callstack (SymbolExpression exprs) >>= \case
          AtomLambda closureStack params body -> runLambda closureStack params body
          _ -> withErr $ EvalError "Invalid syntax"

evaluateWithScope :: CallStack -> [Expression] -> EvalResult
evaluateWithScope callstack = evalExpression callstack . SymbolExpression . (createLabel "do" :)

loadLibraryInto :: String -> CallStack -> ExceptWithEvalError CallStack
loadLibraryInto stdlibStr callstack = snd <$> (except (tokenize stdlibStr) >>= evaluateWithScope callstack)

evaluateWithStdlib :: CallStack -> [Expression] -> EvalResult
evaluateWithStdlib parent exprs = do
  callstack <- loadLibraryInto stdlibContent parent
  evaluateWithScope callstack exprs

evaluate :: [Expression] -> EvalResultPure
evaluate exprs = fst <$> evaluateWithStdlib emptyCallStack exprs

interpret :: CallStack -> String -> EvalResult
interpret callstack = evaluateWithStdlib callstack <=< except . tokenize

--
