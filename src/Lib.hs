module Lib
  ( someFunc,
  )
where

import Parse

someFunc :: IO ()
someFunc = getContents >>= print . parser . lexer
