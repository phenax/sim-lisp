{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Eval where

import Atom
import Control.Monad
import qualified Data.ByteString.Char8 as BChar8
import Data.FileEmbed
import qualified Data.Map as Map
import Errors
import LParser
import Text.RawString.QQ
import Utils

type Scope = Map.Map String Atom

type MacroEvaluator = Scope -> [Expression] -> Either Error (Atom, Scope)

emptyScope = Map.empty

evalConcat :: Scope -> [Expression] -> Either Error [(Atom, Scope)]
evalConcat scope = mergeM . map (evalExpression scope)

foldInts :: Scope -> (Integer -> Integer -> Integer) -> Integer -> [Expression] -> Either Error (Atom, Scope)
foldInts scope fn init = folder scope <=< evalConcat scope
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
doblockE scope = \case
  [] -> Left $ EvalError "Empty `do` block"
  lst -> foldl evaluateExpr (Right (AtomInt 0, scope)) lst
    where
      evaluateExpr = \result expr -> do
        (_, lastScope) <- result
        evalExpression lastScope expr

declareE :: MacroEvaluator
declareE scope = \case
  [Atom (AtomSymbol k), expr] ->
    (\value -> (value, Map.insert k value scope)) <$> evalExpressionPure scope expr
  _ -> Left $ EvalError "Invalid `declare` expression"

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

customMacroE :: String -> MacroEvaluator
customMacroE fn scope = \case
  arguments ->
    let lambda = case Map.lookup fn scope of
          Just (AtomLambda params body) -> Right (params, body)
          _ -> Left $ EvalError ("Invalid call. `" ++ fn ++ "` is not a macro")
        toScope params = (scope `Map.union`) . Map.fromList . zip params . map fst <$> evalConcat scope arguments
     in do
          (params, body) <- lambda
          newScope <- toScope params
          (result, _) <- evalExpression newScope body
          return (result, scope)

importE :: MacroEvaluator
importE scope = \case
  [Atom (AtomString file)] -> Left $ EvalError "TODO: Import impl"
  _ -> Left $ EvalError "Invalid import expression"

compare2E :: [Ordering] -> MacroEvaluator
compare2E ordering scope = \case
  [exp1, exp2] -> do
    a <- evalExpressionPure scope exp1
    b <- evalExpressionPure scope exp2
    return (AtomBool (check a b), scope)
    where
      check = \a b -> compareAtom a b `elem` ordering
  _ -> Left $ EvalError "Invalid number of arguments"

-- Evaluate expression without leaking scope
evalExpressionPure :: Scope -> Expression -> Either Error Atom
evalExpressionPure scope = fmap fst . evalExpression scope

evalExpression :: Scope -> Expression -> Either Error (Atom, Scope)
evalExpression scope = \case
  Atom atom -> case atom of
    AtomSymbol k -> case Map.lookup k scope of
      Just value -> Right (value, scope)
      Nothing -> Left $ EvalError $ "Variable " ++ k ++ " not found in scope"
    a -> Right (a, scope)
  SymbolExpression (Atom (AtomSymbol op) : lst) -> case op of
    "+" -> foldInts scope (+) 0 lst
    "-" -> foldInts scope (-) 0 lst
    "*" -> foldInts scope (*) 1 lst
    "/" -> foldInts scope div 1 lst
    "=" -> compare2E [EQ] scope lst
    "<" -> compare2E [LT] scope lst
    "<=" -> compare2E [LT, EQ] scope lst
    ">" -> compare2E [GT] scope lst
    ">=" -> compare2E [GT, EQ] scope lst
    "lambda" -> lambdaE scope lst
    "do" -> doblockE scope lst
    "declare" -> declareE scope lst
    "let" -> letbindingE scope lst
    "import" -> importE scope lst
    fn -> customMacroE fn scope lst
  _ -> Left $ EvalError "TODO: Not impl out"

evaluateWithScope :: Scope -> [Expression] -> Either Error (Atom, Scope)
evaluateWithScope scope = evalExpression scope . SymbolExpression . makeDo
  where
    makeDo = (Atom (AtomSymbol "do") :)

stdlibContent :: [String]
stdlibContent = map BChar8.unpack [$(embedFile "./src/stdlib/core.sim")]

loadLibrarysIntoScope :: Scope -> Either Error Scope
loadLibrarysIntoScope scope = snd <$> (libraryAst >>= evaluateWithScope scope)
  where
    libraryAst =
      tokenize $ unwords stdlibContent

evaluate :: [Expression] -> Either Error Atom
evaluate exprs = do
  scope <- loadLibrarysIntoScope emptyScope
  (result, _) <- evaluateWithScope scope exprs
  return result

interpret :: String -> IO ()
interpret = print . fmap evaluate . tokenize

--
--
--
--
