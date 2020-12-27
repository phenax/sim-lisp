module Main where

import Eval (interpret)
import System.Environment
import System.IO

ask q = putStr q >> hFlush stdout >> getLine

repl = do
  expr <- ask "(sim) > "
  case expr of
    ":q" -> return ()
    ":quit" -> return ()
    expr -> do
      putStrLn ""
      interpret expr
      repl

interpretFile f = do
  contents <- readFile f
  interpret contents

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> interpretFile "./examples/fibo.scm"
    ["repl"] -> repl
    (file : _) -> interpretFile file
