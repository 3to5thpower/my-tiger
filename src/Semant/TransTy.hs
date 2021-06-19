module Semant.TransTy where

import Data.Foldable (foldlM)
import Data.Map as M (insert, (!?))
import Parse.Data (Id (Id), Type (..))
import Semant.Types as T (TEnv, Ty (Array, Name, Record))

transTy :: T.TEnv -> Type -> Either String (T.TEnv, T.Ty)
transTy tenv (Type (Id name)) = case tenv M.!? name of
  Nothing -> Left $ "undefined type of: " ++ name
  Just t@(T.Name ref) -> case ref of
    Just str -> case tenv M.!? str of
      Just ty -> Right (tenv, ty)
      Nothing -> Left $ "undefined type of \"" ++ name ++ "\""
    Nothing -> Right (M.insert name (T.Name (Just name)) tenv, T.Name (Just name))
  Just ty -> Right (tenv, ty)
transTy tenv (ArrayType name) = do
  (tenv, ty) <- transTy tenv (Type name)
  return (tenv, T.Array ty 0)
transTy tenv (RecordType tyfields) = do
  (e, fields) <-
    foldlM
      ( \(tenv, lst) (Id fieldName, typeId) -> do
          (tenv, ty) <- transTy tenv (Type typeId)
          return (tenv, lst ++ [(fieldName, ty)])
      )
      (tenv, [])
      tyfields
  return (e, T.Record fields 0)