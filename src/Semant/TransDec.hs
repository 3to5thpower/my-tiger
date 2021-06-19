module Semant.TransDec where

import Data.Foldable (foldlM)
import Data.Functor ((<&>))
import qualified Data.Map as M
import Parse.Data
  ( Dec (..),
    FunDec (LongFunDec, ShortFunDec),
    Id (Id),
    Type (ArrayType, RecordType, Type),
    VarDec (LongVarDec, ShortVarDec),
  )
import Semant.Semant (ExpTy (ExpTy, exp, ty), transExp)
import Semant.TransTy (transTy)
import qualified Semant.Types as T

transDec :: T.VEnv -> T.TEnv -> [Dec] -> Either String (T.VEnv, T.TEnv)
transDec v t decs = do
  envs <- insertHeader v t decs
  foldlM trDec envs decs
  where
    insertHeader venv tenv decs = case decs of
      [] -> Right (venv, tenv)
      dec : decs -> case dec of
        VarDec _ -> insertHeader venv tenv decs
        TyDec (Id name) _ ->
          insertHeader venv (M.insert name (T.Name Nothing) tenv) decs
        FunDec (ShortFunDec (Id name) tyfields _) -> do
          tyList <- tyListFromTyFields tenv tyfields
          insertHeader (M.insert name (T.FunEntry tyList T.Unit) venv) tenv decs
        FunDec (LongFunDec (Id name) tyfields (Id retType) _) -> do
          tyList <- tyListFromTyFields tenv tyfields
          ret <- T.actualTy tenv retType
          insertHeader (M.insert name (T.FunEntry tyList ret) venv) tenv decs

    trDec (venv, tenv) dec = case dec of
      TyDec (Id name) tydec -> case tenv M.!? name of
        Just (T.Name (Just ref)) -> Left "multiple definition of type"
        Just (T.Name Nothing) -> do
          (tenv, reftype) <- case tydec of
            Type (Id id) -> Right (tenv, T.Name (Just id))
            RecordType _ -> transTy tenv tydec
            ArrayType _ -> transTy tenv tydec
          return (venv, M.insert name reftype tenv)
        Just _ -> do
          (tenv, ty) <- transTy tenv tydec
          return (venv, M.insert name ty tenv)
        _ -> undefined -- unreachable pattern
      VarDec vdec -> trvardec venv tenv vdec
      FunDec fdec@ShortFunDec {} -> trfundec venv tenv fdec
      FunDec fdec@(LongFunDec (Id name) tyfields (Id retType) _) -> do
        ty <- T.actualTy tenv retType
        args <- tyListFromTyFields tenv tyfields
        trfundec (M.insert name (T.FunEntry args ty) venv) tenv fdec
    trvardec venv tenv vdec = case vdec of
      ShortVarDec (Id name) exp -> case transExp venv tenv exp of
        Right ExpTy {Semant.Semant.exp = _, ty = ty} -> Right (M.insert name (T.VarEntry ty) venv, tenv)
        Left e -> Left e
      LongVarDec (Id name) (Id typeId) exp -> case transExp venv tenv exp of
        Right ExpTy {Semant.Semant.exp = _, ty = inferedTy} -> do
          actualType <- T.actualTy tenv typeId
          if actualType == inferedTy
            then Right (M.insert name (T.VarEntry actualType) venv, tenv)
            else Left $ "Couldn't match expected type: " ++ show inferedTy ++ "but actual: " ++ typeId
        Left e -> Left e
    trfundec venv tenv fdec = case fdec of
      ShortFunDec (Id name) tyfields body -> do
        venv <- insertArgsToEnv venv tenv tyfields
        args <- tyListFromTyFields tenv tyfields
        retType <- transExp venv tenv body <&> ty
        return (M.insert name (T.FunEntry args retType) venv, tenv)
      LongFunDec (Id name) tyfields (Id retTypeId) body -> do
        venv <- insertArgsToEnv venv tenv tyfields
        args <- tyListFromTyFields tenv tyfields
        retType <- transExp venv tenv body <&> ty
        specifiedretType <- T.actualTy tenv retTypeId
        if retType /= specifiedretType
          then
            Left $
              "Couldn't match expected type: "
                ++ show retType
                ++ " but actual: "
                ++ retTypeId
          else return (M.insert name (T.FunEntry args retType) venv, tenv)
    tyListFromTyFields tenv = mapM (idToTy tenv)
      where
        idToTy tenv (Id varId, Id typeId) = case tenv M.!? typeId of
          Nothing -> Left $ "undefined type: \"" ++ typeId ++ " of variable \"" ++ varId ++ "\""
          Just ty -> Right ty
    insertArgsToEnv venv _ [] = Right venv
    insertArgsToEnv venv tenv (x : xs) = do
      insertedvenv <- insertArgToEnv tenv venv x
      insertArgsToEnv insertedvenv tenv xs
      where
        insertArgToEnv tenv venv (Id varId, Id typeId) = case tenv M.!? typeId of
          Nothing -> Left $ "undefined type: \"" ++ typeId ++ "\" of variable \"" ++ varId ++ "\""
          Just ty -> Right $ M.insert varId (T.VarEntry ty) venv
