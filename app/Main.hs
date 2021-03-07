module Main where

import Parse

main :: IO ()
main = do
  s <- getContents
  (print . parse) s
