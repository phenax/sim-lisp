{-# LANGUAGE LambdaCase #-}

module Eval where

import Control.Monad
import Errors
import LParser

data EvalResult
  = EString String
  | EInteger Integer
  | EList [EvalResult]
  deriving (Show, Eq)

mapInt fn = \case
  EInteger x -> EInteger $ fn x
  x -> x

innerConcat :: Either e [a] -> Either e a -> Either e [a]
innerConcat list item = do
  ls <- list
  x <- item
  return $ x : ls

evalExpressions :: [Expression] -> Either Error [EvalResult]
evalExpressions = foldl innerConcat (Right []) . map evalExpression

foldInts :: (Integer -> Integer -> Integer) -> Integer -> [Expression] -> Either Error EvalResult
foldInts fn init = folder <=< evalExpressions
  where
    folder :: [EvalResult] -> Either Error EvalResult
    folder = \case
      [] -> Right $ EInteger init
      [EInteger a] -> Right $ EInteger a
      ((EInteger a) : tail) -> mapInt (`fn` a) <$> folder tail
      _ -> Left $ EvalError "Invalid set of params"

evalExpression :: Expression -> Either Error EvalResult
evalExpression = \case
  (LInteger n) -> Right $ EInteger n
  (LString s) -> Right $ EString s
  (Symbol s) -> Right $ EString s -- TODO: Read variable value
  --(LList []) -> Right $ EList []
  --(LList exprs) -> map (fmap EList . evalExpression) $ exprs
  (SExpression op lst) -> case op of
    Symbol "+" -> foldInts (+) 0 lst
    Symbol "-" -> foldInts (-) 0 lst
    Symbol "*" -> foldInts (*) 1 lst
    Symbol "/" -> foldInts div 1 lst
    Symbol fn -> Left $ EvalError $ "TODO: Not impl (" ++ fn ++ ")"
  _ -> Left $ EvalError "TODO: Not impl"

evaluate = print

interpret :: String -> IO ()
interpret = evaluate . tokenize
