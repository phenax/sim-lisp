{-# LANGUAGE LambdaCase #-}

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

innerConcat :: Either e [a] -> Either e a -> Either e [a]
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

mergeM :: [Either e a] -> Either e [a]
mergeM = foldl innerConcat (Right [])

evalConcat :: Scope -> [Expression] -> Either Error [Atom]
evalConcat scope = mergeM . map (evalExpression scope)

foldInts :: Scope -> (Integer -> Integer -> Integer) -> Integer -> [Expression] -> Either Error Atom
foldInts scope fn init = folder <=< evalConcat scope
  where
    folder :: [Atom] -> Either Error Atom
    folder = \case
      [] -> Right $ AtomInt init
      [AtomInt a] -> Right $ AtomInt a
      ((AtomInt a) : tail) -> mapInt (`fn` a) <$> folder tail
      _ -> Left $ EvalError "Invalid set of params"

letPair :: Expression -> Either Error (String, Expression)
letPair = \case
  SymbolExpression [Atom (AtomSymbol s), expr] -> Right (s, expr)
  _ -> Left $ EvalError "Invalid let binding"

flattenPairBySnd :: [(k, Either e a)] -> Either e [(k, a)]
flattenPairBySnd = foldl innerConcatPair (Right [])

evalExpression :: Scope -> Expression -> Either Error Atom
evalExpression scope = \case
  Atom atom -> case atom of
    AtomInt n -> Right $ AtomInt n
    AtomString s -> Right $ AtomString s
    AtomSymbol k -> case Map.lookup k scope of
      Just value -> Right value
      Nothing -> Left $ EvalError $ "Variable " ++ k ++ " not found in scope"
    _ -> Left $ EvalError "TODO: Atom not implemented"
  SymbolExpression (Atom (AtomSymbol op) : lst) -> case op of
    "+" -> foldInts scope (+) 0 lst
    "-" -> foldInts scope (-) 0 lst
    "*" -> foldInts scope (*) 1 lst
    "/" -> foldInts scope div 1 lst
    -- (lambda (a b c d) (+ a b c d))
    --"lambda" -> case lst of
    --[SymbolExpression args, SymbolExpression body] ->
    ----
    ----
    --Left $ EvalError "TODO: impl"
    "let" -> case lst of
      [SymbolExpression params, expression] -> do
        -- Evaluate params
        paramMap <- (fmap Map.fromList . flattenPairBySnd) <=< (mergeM . map (fmap (mapSnd (evalExpression scope)) . letPair)) $ params
        -- Inject params into scope
        -- Evaluate body with newScope
        let newScope = paramMap `Map.union` scope
         in evalExpression newScope expression
      _ -> Left $ EvalError "Invalid `let` expression"
    fn -> Left $ EvalError $ "TODO: Macro not implemented (" ++ fn ++ ")"
  _ -> Left $ EvalError "TODO: Not impl out"

evaluate :: [Expression] -> [Either Error Atom]
evaluate = map (evalExpression emptyScope)

interpret :: String -> IO ()
interpret = print . fmap evaluate . tokenize

--
--
--
--
