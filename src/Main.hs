module Main where

import LParser (tokenize)
import System.IO

ask q = putStr q >> hFlush stdout >> getLine

repl = do
  expr <- ask "(sim) > "
  case expr of
    ":q" -> return ()
    ":quit" -> return ()
    expr -> do
      putStrLn ""
      print . tokenize $ expr
      repl

main :: IO ()
main = repl
