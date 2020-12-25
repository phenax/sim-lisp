module Main where

import LParser (tokenize)

main :: IO ()
main = do
  print (tokenize "\"some content\"")
  print (tokenize "1371")
