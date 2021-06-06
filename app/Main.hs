module Main where

import Parse
import Semant

main :: IO ()
main = do
  s <- getContents
  print (parse s)
  print (parse s >>= semant)