module IR.Tree where

type Label = String

data Exp
  = Const Int
  | Temp Int
  | Name Label
  | Binop Bop Exp Exp
  | Mem Exp
  | Arr Exp Exp
  | Rcd Exp Int
  | Call Exp [Exp]
  | Eseq Stm Exp
  deriving (Eq)

data Stm
  = Move Exp Exp
  | Exp Exp
  | Jump Exp [Label]
  | CJump Rop Exp Exp Label Label
  | Seq Stm Stm
  | Label Label
  deriving (Eq)

data Bop
  = Plus
  | Minus
  | Mul
  | Div
  | And
  | Or
  | LShift
  | RShift
  | ARShift
  | Xor
  deriving (Eq)

data Rop
  = Eq
  | Ne
  | Lt
  | Le
  | Gt
  | Ge
  | ULt
  | ULe
  | UGt
  | UGe
  deriving (Eq)