module Main where

import Control.Monad.Trans.Except
import Eval (emptyScope, interpret)
import System.Environment
import System.IO

ask q = putStr q >> hFlush stdout >> getLine

repl pScope = do
  expr <- ask "(sim) > "
  case expr of
    ":q" -> return ()
    ":quit" -> return ()
    expr -> do
      putStrLn ""
      result <- runExceptT $ interpret pScope expr
      case result of
        Right (result, scope) -> do
          print result
          repl scope
        Left e -> do
          print e
          repl pScope

interpretFile f = do
  contents <- readFile f
  result <- runExceptT $ interpret emptyScope contents
  case result of
    Right (result, _) -> print result
    Left e -> print e

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> repl emptyScope
    ["repl"] -> repl emptyScope
    (file : _) -> interpretFile file
