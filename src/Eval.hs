{-# LANGUAGE LambdaCase #-}

module Eval where

import Control.Monad
import qualified Data.Map as Map
import Errors
import LParser

data EvalResult
  = EString String
  | EInteger Integer
  | EList [EvalResult]
  deriving (Show, Eq)

type Context = Map.Map String Atom

newtype Scope = Scope Context

emptyScope = Scope Map.empty

mapInt fn = \case
  EInteger x -> EInteger $ fn x
  x -> x

innerConcat :: Either e [a] -> Either e a -> Either e [a]
innerConcat list item = do
  ls <- list
  x <- item
  return $ x : ls

evalConcat :: Scope -> [Expression] -> Either Error [EvalResult]
evalConcat scope = foldl innerConcat (Right []) . map (evalExpression scope)

foldInts :: Scope -> (Integer -> Integer -> Integer) -> Integer -> [Expression] -> Either Error EvalResult
foldInts scope fn init = folder <=< evalConcat scope
  where
    folder :: [EvalResult] -> Either Error EvalResult
    folder = \case
      [] -> Right $ EInteger init
      [EInteger a] -> Right $ EInteger a
      ((EInteger a) : tail) -> mapInt (`fn` a) <$> folder tail
      _ -> Left $ EvalError "Invalid set of params"

evalExpression :: Scope -> Expression -> Either Error EvalResult
evalExpression scope = \case
  Atom atom -> case atom of
    AtomInt n -> Right $ EInteger n
    AtomString s -> Right $ EString s
    _ -> Left $ EvalError "TODO: Atom not implemented"
  SymbolExpression (Atom (AtomSymbol op) : lst) -> case op of
    "+" -> foldInts scope (+) 0 lst
    "-" -> foldInts scope (-) 0 lst
    "*" -> foldInts scope (*) 1 lst
    "/" -> foldInts scope div 1 lst
    --Symbol "let" ->
    fn -> Left $ EvalError $ "TODO: Macro not implemented (" ++ fn ++ ")"
  _ -> Left $ EvalError "TODO: Not impl out"

evaluate :: [Expression] -> [Either Error EvalResult]
evaluate = map (evalExpression emptyScope)

interpret :: String -> IO ()
interpret = print . fmap evaluate . tokenize

--
--
--
--
