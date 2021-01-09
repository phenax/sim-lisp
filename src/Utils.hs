{-# LANGUAGE LambdaCase #-}

module Utils where

import Atom
import Control.Monad
import Debug.Trace
import Errors

liftJoin2 :: (Monad m) => (a -> b -> m c) -> m a -> m b -> m c
liftJoin2 fn m1 m2 = m1 >>= (\a -> m2 >>= fn a)

monadPrepend :: Monad m => m [a] -> m a -> m [a]
monadPrepend = liftJoin2 (\ls -> return . (: ls))

monadAppend :: Monad m => m [a] -> m a -> m [a]
monadAppend = liftJoin2 (\ls x -> return $ ls ++ [x])

innerPrependPair :: Monad m => m [(a, b)] -> (a, m b) -> m [(a, b)]
innerPrependPair list item = liftJoin2 (\ls x -> return $ (fst item, x) : ls) list $ snd item

rconcatM :: Monad f => [f a] -> f [a]
rconcatM = foldl monadPrepend (pure [])

concatM :: Monad f => [f a] -> f [a]
concatM = foldl monadAppend (pure [])

flattenPairBySnd :: [(k, ExceptWithEvalError a)] -> ExceptWithEvalError [(k, a)]
flattenPairBySnd = foldl innerPrependPair (pure [])

fromMaybe :: Maybe a -> ExceptWithEvalError a
fromMaybe = \case
  Just x -> pure x
  Nothing -> withErr $ EvalError "Invalid syntax"
