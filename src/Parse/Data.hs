module Parse.Data
  ( Decs,
    Dec (..),
    Type (..),
    TypeId (..),
    TyFields,
    VarDec (..),
    FunDec (..),
    Exp (..),
    LValue (..),
    Id (..),
  )
where

type Decs = [Dec]

data Dec
  = TyDec TypeId Type
  | VarDec VarDec
  | FunDec FunDec
  deriving (Eq, Show)

data Type = Type TypeId | RecordType TyFields | ArrayType TypeId deriving (Eq, Show)

newtype TypeId = TypeId String deriving (Eq, Show)

type TyFields = [(Id, TypeId)]

data VarDec
  = ShortVarDec Id Exp
  | LongVarDec Id TypeId Exp
  deriving (Eq, Show)

data FunDec
  = ShortFunDec Id TyFields Exp
  | LongFunDec Id TyFields TypeId Exp
  deriving (Eq, Show)

data LValue
  = Variable Id
  | DotAccess LValue Id
  | Index LValue Exp
  deriving (Eq, Show)

newtype Id = Id String deriving (Eq, Show)

data Exp
  = LValue LValue
  | Nil
  | Seq [Exp]
  | Unit
  | Int Int
  | String String
  | FunCall Id [Exp]
  | Negate Exp
  | Plus Exp Exp
  | Minus Exp Exp
  | Times Exp Exp
  | Div Exp Exp
  | Less Exp Exp
  | LessEqual Exp Exp
  | Greater Exp Exp
  | GreaterEqual Exp Exp
  | Equal Exp Exp
  | NotEqual Exp Exp
  | And Exp Exp
  | Or Exp Exp
  | Record TypeId [(Id, Exp)]
  | Array TypeId Exp Exp
  | IfThen Exp Exp
  | IfThenElse Exp Exp Exp
  | WhileDo Exp Exp
  | ForToDo Id Exp Exp Exp
  | Break
  | LetInEnd Decs Exp
  | Brack Exp
  deriving (Eq, Show)
