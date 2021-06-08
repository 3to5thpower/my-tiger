module Semant.Types (Ty (..), Symbol, EnvEntry (..), baseTypesEnv, baseDataEnv, VEnv, TEnv, find) where

import qualified Data.Map as M

type Symbol = String

data EnvEntry
  = VarEntry {ty :: Ty}
  | FunEntry {formals :: [Ty], result :: Ty}
  deriving (Eq, Show)

type VEnv = M.Map Symbol EnvEntry

type TEnv = M.Map Symbol Ty

find :: TEnv -> [Char] -> Either [Char] Ty
find tenv name = case tenv M.!? name of
  Nothing -> Left $ "undefined type of \"" ++ name
  Just ty -> Right ty

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
  | Name Symbol (Maybe Ty)
  deriving (Eq, Show)
