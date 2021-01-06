{-# LANGUAGE LambdaCase #-}

module Utils where

import Atom
import Debug.Trace
import Errors

mapFst fn (a, b) = (fn a, b)

mapSnd fn (a, b) = (a, fn b)

reverseMonadConcat :: Monad m => m [a] -> m a -> m [a]
reverseMonadConcat list item = do
  ls <- list
  x <- item
  return $ x : ls

monadConcat :: Monad m => m [a] -> m a -> m [a]
monadConcat m1 m2 = do
  a <- m1
  b <- m2
  return $ a ++ [b]

-- TODO: Refactor both to use an innerConcatBy
innerConcatPair :: Monad m => m [(a, b)] -> (a, m b) -> m [(a, b)]
innerConcatPair list item = do
  ls <- list
  x <- snd item
  return $ (fst item, x) : ls

rmergeM :: Monad f => [f a] -> f [a]
rmergeM = foldl reverseMonadConcat (pure [])

mergeM :: Monad f => [f a] -> f [a]
mergeM = foldl monadConcat (pure [])

flattenPairBySnd :: [(k, ExceptWithEvalError a)] -> ExceptWithEvalError [(k, a)]
flattenPairBySnd = foldl innerConcatPair (pure [])

toEither :: Maybe a -> ExceptWithEvalError a
toEither = \case
  Just x -> pure x
  Nothing -> withErr $ EvalError "Invalid syntax"
