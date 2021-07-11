module IR.AST where

data Expr = Expr Term ExprOp Expr | ExprTerm Term deriving(Show)
data Term = Term Number TermOp Term
  | TermNumber Number 
  deriving (Show)
newtype Number = Number Integer deriving(Show)
data ExprOp  = Add | Sub deriving(Show)
data TermOp = Mul | Div deriving(Show)
