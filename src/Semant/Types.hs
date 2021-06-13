{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Semant.Types (Ty (..), Symbol, EnvEntry (..), baseTypesEnv, baseDataEnv, VEnv, TEnv) where

import Control.Monad
import Control.Monad.ST
import qualified Data.Map as M
import Data.Maybe (isJust)
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

data Ty where
  Int :: Ty
  String :: Ty
  Record :: [(Symbol, Ty)] -> Unique -> Ty
  Array :: Ty -> Unique -> Ty
  Nil :: Ty
  Unit :: Ty
  Name :: Maybe Symbol -> Ty
  deriving (Show)

instance Eq Ty where
  Int == Int = True
  String == String = True
  Nil == Record _ _ = True
  Record _ _ == Nil = True
  Record r _ == Record s _ = r == s
  Array a _ == Array b _ = a == b
  Nil == Nil = True
  Unit == Unit = True
  Name m == Name n = m == n
  _ == _ = False