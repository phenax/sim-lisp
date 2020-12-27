{-# LANGUAGE LambdaCase #-}

module Utils where

import Errors

mapSnd fn (a, b) = (a, fn b)

innerConcat :: Monad m => m [a] -> m a -> m [a]
innerConcat list item = do
  ls <- list
  x <- item
  return $ x : ls

-- TODO: Refactor both to use an innerConcatBy
innerConcatPair :: Monad m => m [(a, b)] -> (a, m b) -> m [(a, b)]
innerConcatPair list item = do
  ls <- list
  x <- snd item
  return $ (fst item, x) : ls

mergeM :: Monad f => [f a] -> f [a]
mergeM = foldl innerConcat (return [])

flattenPairBySnd :: [(k, Either e a)] -> Either e [(k, a)]
flattenPairBySnd = foldl innerConcatPair (Right [])

toEither :: Maybe a -> Either Error a
toEither = \case
  Just x -> Right x
  Nothing -> Left $ EvalError "Invalid syntax"
