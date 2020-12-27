{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Eval where

import Control.Monad
import qualified Data.Map as Map
import Errors
import LParser

type Scope = Map.Map String Atom

emptyScope = Map.empty

mapSnd fn (a, b) = (a, fn b)

mapInt fn = \case
  AtomInt x -> AtomInt $ fn x
  x -> x

innerConcat :: Monad f => f [a] -> f a -> f [a]
innerConcat list item = do
  ls <- list
  x <- item
  return $ x : ls

-- TODO: Refactor both to use an innerConcatBy
innerConcatPair :: Either e [(a, b)] -> (a, Either e b) -> Either e [(a, b)]
innerConcatPair list item = do
  ls <- list
  x <- snd item
  return $ (fst item, x) : ls

mergeM :: Monad f => [f a] -> f [a]
mergeM = foldl innerConcat (return [])

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

letPair :: Expression -> Either Error (String, Expression)
letPair = \case
  SymbolExpression [Atom (AtomSymbol s), expr] -> Right (s, expr)
  _ -> Left $ EvalError "Invalid `let` binding"

flattenPairBySnd :: [(k, Either e a)] -> Either e [(k, a)]
flattenPairBySnd = foldl innerConcatPair (Right [])

evalExpressionPure :: Scope -> Expression -> Either Error Atom
evalExpressionPure scope = fmap fst . evalExpression scope

isSymbol :: Expression -> Bool
isSymbol = \case
  Atom (AtomSymbol _) -> True
  _ -> False

toSymbolString :: Expression -> Maybe String
toSymbolString = \case
  Atom (AtomSymbol s) -> Just s
  _ -> Nothing

toEither :: Maybe a -> Either Error a
toEither = \case
  Just x -> Right x
  Nothing -> Left $ EvalError "Invalid syntax"

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
    -- (lambda (a b c d) (+ a b c d))
    "lambda" -> case lst of
      [SymbolExpression args, body] ->
        if all isSymbol args
          then (\params -> (AtomLambda params body, scope)) <$> (toEither . mergeM . map toSymbolString) args
          else Left $ EvalError "Invalid arguments passed to `lambda` expression"
      _ -> Left $ EvalError "Invalid `lambda` expression"
    "do" -> case lst of
      [] -> Left $ EvalError "Empty `do` block"
      lst -> foldl evaluateExpr (Right (AtomInt 0, scope)) lst
        where
          evaluateExpr = \result expr -> do
            (_, lastScope) <- result
            evalExpression lastScope expr
    "declare" -> case lst of
      [Atom (AtomSymbol k), expr] ->
        (\value -> (value, Map.insert k value scope)) <$> evalExpressionPure scope expr
      _ -> Left $ EvalError "Invalid `declare` expression"
    "let" -> case lst of
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
    fn -> case lst of
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
