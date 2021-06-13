module Semant.Types (Ty (..), Symbol, EnvEntry (..), baseTypesEnv, baseDataEnv, VEnv, TEnv) where

import Control.Monad.ST
import qualified Data.Map as M
import Data.STRef

type Symbol = String

data EnvEntry
  = VarEntry {ty :: Ty}
  | FunEntry {formals :: [Ty], result :: Ty}
  deriving (Eq, Show)

type VEnv = M.Map Symbol EnvEntry

type TEnv = M.Map Symbol Ty

baseTypesEnv :: M.Map Symbol Ty
baseTypesEnv = M.fromList [("int", Int), ("string", String)]

baseDataEnv :: M.Map Symbol EnvEntry
baseDataEnv =
  M.fromList
    [ ("print", FunEntry [String] Unit),
      ("flush", FunEntry [] Unit),
      ("getchar", FunEntry [] String),
      ("ord", FunEntry [String] Int),
      ("chr", FunEntry [Int] String),
      ("size", FunEntry [String] Int),
      ("substr", FunEntry [String, Int, Int] String),
      ("concat", FunEntry [String, String] String),
      ("not", FunEntry [Int] Int),
      ("exit", FunEntry [Int] Unit)
    ]

type Unique = Integer

data Ty
  = Int
  | String
  | Record [(Symbol, Ty)] Unique
  | Array Ty Unique
  | Nil
  | Unit
  | Name Symbol (STRef (*) (Maybe Ty))

instance Show Ty where
  show Int = "Int"
  show String = "String"
  show (Record lst _) = "Record" ++ show lst
  show (Array ty _) = "Array of " ++ show ty
  show Nil = "Nil"
  show Unit = "()"

instance Eq Ty where
  Int == Int = True
  String == String = True
  Record r _ == Record s _ = r == s
  Array a _ == Array b _ = a == b
  Nil == Nil = True
  Unit == Unit = True
