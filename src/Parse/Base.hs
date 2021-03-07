module Parse.Base (parse) where

import Parse.Data
import Parse.Lexer
import Parse.Parser

parse :: String -> Either String Exp
parse = parser