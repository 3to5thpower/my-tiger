module Main where

import Parse
import Semant

main :: IO ()
main = do
  s <- getContents
  let ast = parse s
  print ast
  print (ast >>= semant)