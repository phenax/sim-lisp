module Eval where

import Errors
import LParser

data EvalResult
  = EString String
  | EInteger Integer
  | EList [EvalResult]
  deriving (Show, Eq)

runIntOp2 fn lst = case map evalExpression lst of
  [Right (EInteger n)] -> Right $ EInteger n
  [Right (EInteger a), Right (EInteger b)] -> Right $ fn a b
  _ -> Left $ EvalError "TODO: Not impl"

--runIntOp fn lst = case map evalExpression lst of
--[Right (EInteger n)] -> Right $ EInteger n
--(Right (EInteger n):ls) -> Right $ EInteger $ foldl (\acc -> fmap (\x -> acc)) (Right (EInteger n)) ls
--_ -> Left "TODO: Not impl"

evalExpression :: Expression -> Either EvalError EvalResult
evalExpression expr = case expr of
  (LInteger n) -> Right $ EInteger n
  (LString s) -> Right $ EString s
  (Symbol s) -> Right $ EString s -- TODO: Read variable value
  --(LList []) -> Right $ EList []
  --(LList exprs) -> map (fmap EList . evalExpression) $ exprs
  (SExpression op lst) -> case op of
    Symbol "+" -> runIntOp2 (\a -> EInteger . (+ a)) lst
    Symbol "-" -> runIntOp2 (\a -> EInteger . (-) a) lst
    Symbol "*" -> runIntOp2 (\a -> EInteger . (* a)) lst
    Symbol "/" -> runIntOp2 (\a -> EInteger . div a) lst
    Symbol fn -> Left $ EvalError $ "TODO: Not impl (" ++ fn ++ ")"
  _ -> Left $ EvalError "TODO: Not impl"

evaluate = print

interpret :: String -> IO ()
interpret = evaluate . tokenize
