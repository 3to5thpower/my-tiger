{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Semant.Types (Ty (..), Symbol, EnvEntry (..), baseTypesEnv, baseDataEnv, VEnv, TEnv, actualTy) where

import qualified Data.Map as M

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

actualTy :: TEnv -> [Char] -> Either [Char] Ty
actualTy tenv name = actualTy' tenv name []
  where
    actualTy' tenv name searched
      | name `elem` searched = Left "cycle definition"
      | otherwise = case tenv M.!? name of
        Nothing -> Left $ "undefined type of \"" ++ name ++ "\""
        Just t@(Name ref) -> case ref of
          Just str -> actualTy' tenv str $ name : searched
          Nothing -> Left $ "undefined type of \"" ++ name ++ "\""
        Just ty -> Right ty
