module Main where

import Parse

main :: IO ()
main = getContents >>= print . parse
