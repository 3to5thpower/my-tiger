module Main where

import Parse

p' (Right []) = putStr "\n"
p' (Right (result : rest)) = do
  print result
  p' $ Right rest
p' (Left s) = putStrLn s

main :: IO ()
main = do
  s <- getContents
  (print . parse) s
