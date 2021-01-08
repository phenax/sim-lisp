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
  [ ("add", intBinaryOpt (+) id),
    ("mul", intBinaryOpt (*) id),
    ("sub", intBinaryOpt (-) (* (-1))),
    ("div", intBinaryOpt div id),
    ("mod", intBinaryOpt mod id),
    ("eq?", compare2E [EQ]),
    ("lt?", compare2E [LT]),
    ("lte?", compare2E [LT, EQ]),
    ("gt?", compare2E [GT]),
    ("gte?", compare2E [GT, EQ]),
    ("quote", quoteE),
    ("eval", evalE),
    ("lambda", lambdaE),
    ("cons", consE),
    ("car", carE),
    ("cdr", cdrE),
    ("if", ifE),
    ("do", doblockE),
    ("def", defineFunctionE),
    ("declare", defineFunctionE),
    ("apply", applyExpressionE),
    ("let", letbindingE),
    ("number?", isNumberE),
    ("boolean?", isBooleanE),
    ("display", displayE),
    ("import", importE),
    ("def-syntax", syntaxDefinitionE)
  ]

syntaxDefinitionE :: Evaluator
syntaxDefinitionE scope = \case
  (Atom (AtomSymbol (Atom (AtomLabel name))) : syntaxes) ->
    pure (syntax, Map.insert name syntax scope)
    where
      syntax = AtomSyntax name (map toSyntaxPair syntaxes)
      toSyntaxPair = \case
        SymbolExpression [SymbolExpression syntax, body] -> (SymbolExpression syntax, body)
        _ -> (Atom AtomNil, Atom AtomNil)
  _ -> withErr $ EvalError "Invalid syntax definition"

displayE :: Evaluator
displayE scope exprs = do
  result <- foldl monadAppend (pure []) . map (fmap show . evalExpressionPure scope) $ exprs
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
evalConcat scope = rconcatM . map (evalExpression scope)

intBinaryOpt :: (Integer -> Integer -> Integer) -> (Integer -> Integer) -> Evaluator
intBinaryOpt binaryOp unaryOp scope expr = evalConcat scope expr >>= runOp . map fst
  where
    runOp :: [Atom] -> ExceptWithEvalError (Atom, Scope)
    runOp = \case
      [AtomInt a, AtomInt b] -> pure (AtomInt $ binaryOp b a, scope)
      [AtomInt a] -> pure (AtomInt $ unaryOp a, scope)
      _ -> withErr $ EvalError "Invalid set of params"

lambdaE :: Evaluator
lambdaE scope = \case
  [SymbolExpression args, body] ->
    if all isSymbol args
      then (\params -> (AtomLambda params body, scope)) <$> (toEither . concatM . map toSymbolString) args
      else withErr $ EvalError "Invalid arguments passed to `lambda` expression"
  _ -> withErr $ EvalError "Invalid `lambda` expression"

doblockE :: Evaluator
doblockE scope = foldl evaluateExpr (pure (AtomInt 0, scope))
  where
    evaluateExpr :: EvalResult -> Expression -> EvalResult
    evaluateExpr result expr = result >>= (`evalExpression` expr) . snd

defineFunctionE :: Evaluator
defineFunctionE scope = \case
  [Atom (AtomSymbol (Atom (AtomLabel k))), expr] ->
    (\value -> (value, Map.insert k value scope)) <$> evalExpressionPure scope expr
  [Atom (AtomSymbol (Atom (AtomLabel name))), SymbolExpression args, body] ->
    if all isSymbol args
      then fmap defineLambda . toEither . concatM . map toSymbolString $ args
      else withErr $ EvalError "Invalid arguments passed to `lambda` expression"
    where
      defineLambda params =
        let lambda = AtomLambda params body
         in (lambda, Map.insert name lambda scope)
  _ -> withErr $ EvalError "Invalid `def` expression"

resolveScope :: ExceptWithEvalError Scope -> ExceptWithEvalError (String, Expression) -> ExceptWithEvalError Scope
resolveScope sc binding = do
  scope' <- sc
  (name, expr) <- binding
  value <- evalExpressionPure scope' expr
  return $ Map.insert name value scope'

letbindingE :: Evaluator
letbindingE scope = \case
  [SymbolExpression params, expression] -> do
    newScope <- foldl resolveScope (pure scope) . map letPair $ params
    evalExpression newScope expression
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
      check = \a b -> compare a b `elem` ordering
  _ -> withErr $ EvalError "Invalid number of arguments passed for comparison"

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
      _ -> withErr $ EvalError "Invalid argument passed to `eval`"
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
  [expr1, expr2] -> mMerge2 prependAtoms (evalExpressionPure scope expr1) (evalExpressionPure scope expr2)
    where
      prependAtoms a b = case b of
        AtomNil -> pure (AtomSymbol $ SymbolExpression [Atom a], scope)
        AtomSymbol (SymbolExpression ls) -> pure (AtomSymbol $ SymbolExpression $ Atom a : ls, scope)
        _ -> withErr $ EvalError "Invalid argument passed to `cons`"
  _ -> withErr $ EvalError "Invalid number of arguments passed to `cons`"

toLambdaScope :: [String] -> Scope -> [Expression] -> ExceptWithEvalError Scope
toLambdaScope params scope args =
  (`Map.union` scope) . Map.fromList . zipParams params . map fst <$> concatM (map (evalExpression scope) args)
  where
    zipParams :: [String] -> [Atom] -> [(String, Atom)]
    zipParams ps argValues = case ps of
      [] -> []
      [param] -> [(param, head argValues)]
      ["...", restP] -> [(restP, listExpr)]
        where
          listExpr =
            if null argValues
              then AtomNil
              else AtomSymbol . SymbolExpression . map Atom $ argValues
      (param : rest) -> (param, head argValues) : zipParams rest (tail argValues)

applyLambda :: [String] -> Expression -> Evaluator
applyLambda params body scope arguments = do
  newScope <- toLambdaScope params scope arguments
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

fnCallE :: String -> Evaluator
fnCallE fn scope arguments =
  flatten $ applyAsSymbol fn scope arguments <|> applyBuiltin fn scope arguments
  where
    flatten = \case
      Just e -> e
      Nothing -> withErr $ EvalError ("Invalid function call : " ++ fn)

applyExpressionE :: Evaluator
applyExpressionE scope = \case
  [fn, arguments] -> do
    argType <- evalExpressionPure scope arguments
    case argType of
      AtomSymbol (SymbolExpression args) -> evalExpression scope $ SymbolExpression (fn : args)
      _ -> withErr $ EvalError "Invalid args: apply expected a list of arguments"
  ls -> withErr $ EvalError ("Invalid number of arguments passed to apply::" ++ show ls)

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
  SymbolExpression exprs ->
    let lst = tail exprs
     in case head exprs of
          -- TODO: predefined function as symbol
          Atom (AtomSymbol (Atom (AtomLabel opSymbol))) -> fnCallE opSymbol scope lst
          SymbolExpression exprs ->
            evalExpressionPure scope (SymbolExpression exprs) >>= \case
              AtomLambda params body -> applyLambda params body scope lst
              _ -> withErr $ EvalError "Invalid syntax"
          _ -> withErr $ EvalError "TODO: Not impl 1"

evaluateWithScope :: Scope -> [Expression] -> EvalResult
evaluateWithScope scope = evalExpression scope . SymbolExpression . (createLabel "do" :)

loadLibraryIntoScope :: String -> Scope -> ExceptWithEvalError Scope
loadLibraryIntoScope stdlibStr scope = snd <$> (except (tokenize stdlibStr) >>= evaluateWithScope scope)

evaluateWithStdlib :: Scope -> [Expression] -> EvalResult
evaluateWithStdlib parentScope exprs = do
  scope <- loadLibraryIntoScope stdlibContent parentScope
  evaluateWithScope scope exprs

evaluate :: [Expression] -> EvalResultPure
evaluate exprs = fst <$> evaluateWithStdlib emptyScope exprs

interpret :: Scope -> String -> EvalResult
interpret scope = evaluateWithStdlib scope <=< except . tokenize

--
