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

data ContextType = Global | Macro Expression

data Context = Context
  { kind :: ContextType,
    symbols :: [Integer]
  }

newtype Stack = Stack [Context]

-- evalExpression :: Context -> Expression -> Either Error (EvalResult, Context)
-- evaluate :: Context -> [Expression] -> Either Error (EvalResult, Context)

mapInt fn = \case
  EInteger x -> EInteger $ fn x
  x -> x

innerConcat :: Either e [a] -> Either e a -> Either e [a]
innerConcat list item = do
  ls <- list
  x <- item
  return $ x : ls

evalConcat :: [Expression] -> Either Error [EvalResult]
evalConcat = foldl innerConcat (Right []) . map evalExpression

foldInts :: (Integer -> Integer -> Integer) -> Integer -> [Expression] -> Either Error EvalResult
foldInts fn init = folder <=< evalConcat
  where
    folder :: [EvalResult] -> Either Error EvalResult
    folder = \case
      [] -> Right $ EInteger init
      [EInteger a] -> Right $ EInteger a
      ((EInteger a) : tail) -> mapInt (`fn` a) <$> folder tail
      _ -> Left $ EvalError "Invalid set of params"

evalExpression :: Expression -> Either Error EvalResult
evalExpression = \case
  Atom atom -> case atom of
    AtomInt n -> Right $ EInteger n
    AtomString s -> Right $ EString s
    _ -> Left $ EvalError "TODO: Not impl"
  SymbolExpression op lst -> case op of
    Symbol "+" -> foldInts (+) 0 lst
    Symbol "-" -> foldInts (-) 0 lst
    Symbol "*" -> foldInts (*) 1 lst
    Symbol "/" -> foldInts div 1 lst
    Symbol fn -> Left $ EvalError $ "TODO: Not impl (" ++ fn ++ ")"

evaluate :: [Expression] -> [Either Error EvalResult]
evaluate = map evalExpression

interpret :: String -> IO ()
interpret = print . fmap evaluate . tokenize

--
--
--
--
