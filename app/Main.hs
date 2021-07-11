module Main where

import Parse
import Semant

import IR.AST
import IR.Compiler

main :: IO ()
main = do
  let ast = Expr (Term (Number 1) Mul (TermNumber (Number 3))) 
                 Add (ExprTerm (Term (Number 2) Div (TermNumber (Number 4))))
  let out = compile ast
  print out