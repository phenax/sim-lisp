{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Eval where

import Atom
import Control.Monad
import qualified Data.Map as Map
import Errors
import LParser
import Utils

type Scope = Map.Map String Atom

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

evalExpressionPure :: Scope -> Expression -> Either Error Atom
evalExpressionPure scope = fmap fst . evalExpression scope

lambdaE :: Scope -> [Expression] -> Either Error (Atom, Scope)
lambdaE scope = \case
  [SymbolExpression args, body] ->
    if all isSymbol args
      then (\params -> (AtomLambda params body, scope)) <$> (toEither . mergeM . map toSymbolString) args
      else Left $ EvalError "Invalid arguments passed to `lambda` expression"
  _ -> Left $ EvalError "Invalid `lambda` expression"

doblockE :: Scope -> [Expression] -> Either Error (Atom, Scope)
doblockE scope = \case
  [] -> Left $ EvalError "Empty `do` block"
  lst -> foldl evaluateExpr (Right (AtomInt 0, scope)) lst
    where
      evaluateExpr = \result expr -> do
        (_, lastScope) <- result
        evalExpression lastScope expr

declareE :: Scope -> [Expression] -> Either Error (Atom, Scope)
declareE scope = \case
  [Atom (AtomSymbol k), expr] ->
    (\value -> (value, Map.insert k value scope)) <$> evalExpressionPure scope expr
  _ -> Left $ EvalError "Invalid `declare` expression"

letbindingE :: Scope -> [Expression] -> Either Error (Atom, Scope)
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

customMacroE :: String -> Scope -> [Expression] -> Either Error (Atom, Scope)
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

importE :: Scope -> [Expression] -> Either Error (Atom, Scope)
importE scope = \case
  [Atom (AtomString file)] -> Left $ EvalError "TODO: Import impl"
  _ -> Left $ EvalError "Invalid import expression"

evalExpression :: Scope -> Expression -> Either Error (Atom, Scope)
evalExpression scope = \case
  Atom atom -> case atom of
    AtomInt n -> Right (AtomInt n, scope)
    AtomString s -> Right (AtomString s, scope)
    AtomSymbol k -> case Map.lookup k scope of
      Just value -> Right (value, scope)
      Nothing -> Left $ EvalError $ "Variable " ++ k ++ " not found in scope"
    _ -> Left $ EvalError "TODO: Atom not implemented"
  SymbolExpression (Atom (AtomSymbol op) : lst) -> case op of
    "+" -> foldInts scope (+) 0 lst
    "-" -> foldInts scope (-) 0 lst
    "*" -> foldInts scope (*) 1 lst
    "/" -> foldInts scope div 1 lst
    "lambda" -> lambdaE scope lst
    "do" -> doblockE scope lst
    "declare" -> declareE scope lst
    "let" -> letbindingE scope lst
    "import" -> importE scope lst
    fn -> customMacroE fn scope lst
  _ -> Left $ EvalError "TODO: Not impl out"

evaluate :: [Expression] -> Either Error Atom
evaluate = evalExpressionPure emptyScope . SymbolExpression . makeDo
  where
    makeDo = (Atom (AtomSymbol "do") :)

interpret :: String -> IO ()
interpret = print . fmap evaluate . tokenize

--
--
--
--
