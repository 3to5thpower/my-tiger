module Parse.Base (parse) where

import Parse.Data
import Parse.Lexer
import Parse.Parser

parse :: String -> Exp
parse = parser . lexer