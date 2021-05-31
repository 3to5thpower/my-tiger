module Semant.Types (Ty (..), Symbol, EnvEntry (..), baseTypesEnv, baseDataEnv) where

import qualified Data.Map as Map

type Symbol = String

type Table = Map.Map Symbol

data EnvEntry
  = VarEntry {ty :: Ty}
  | FunEntry {formals :: [Ty], result :: Ty}
  deriving (Eq, Show)

baseTypesEnv :: Map.Map Symbol Ty
baseTypesEnv = Map.fromList [("int", Int), ("string", String)]

baseDataEnv :: Map.Map Symbol EnvEntry
baseDataEnv =
  Map.fromList
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
  | Name Symbol (Maybe Ty)
  deriving (Eq, Show)
