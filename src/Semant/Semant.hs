{-# LANGUAGE MultiWayIf #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Semant.Semant where

import Control.Monad.ST
import Data.Foldable (foldlM)
import Data.Functor ((<&>))
import qualified Data.Map as M
import Data.STRef
import Parse.Data
import qualified Semant.Types as T

data ExpTy = ExpTy {exp :: Exp, ty :: T.Ty} deriving (Show, Eq)

semant :: Exp -> Either String ExpTy
semant = transExp T.baseDataEnv T.baseTypesEnv

transExp :: T.VEnv -> T.TEnv -> Exp -> Either String ExpTy
transExp venv tenv exp = trexp exp
  where
    trexp exp = case exp of
      --- atom
      Nil -> Right $ ExpTy Nil T.Nil
      Unit -> Right $ ExpTy Unit T.Unit
      n@(Int _) -> Right $ ExpTy n T.Int
      s@(String _) -> Right $ ExpTy s T.String
      --- operator for int
      Plus e1 e2 -> checkDoubleArgsInt e1 e2
      Minus e1 e2 -> checkDoubleArgsInt e1 e2
      Times e1 e2 -> checkDoubleArgsInt e1 e2
      Div e1 e2 -> checkDoubleArgsInt e1 e2
      Less e1 e2 -> checkDoubleArgsInt e1 e2
      LessEqual e1 e2 -> checkDoubleArgsInt e1 e2
      Greater e1 e2 -> checkDoubleArgsInt e1 e2
      GreaterEqual e1 e2 -> checkDoubleArgsInt e1 e2
      Equal e1 e2 -> checkDoubleArgsInt e1 e2
      NotEqual e1 e2 -> checkDoubleArgsInt e1 e2
      And e1 e2 -> checkDoubleArgsInt e1 e2
      Or e1 e2 -> checkDoubleArgsInt e1 e2
      Negate e -> checkInt e <&> ExpTy exp
      --- other pure expression
      Seq [] -> Right $ ExpTy exp T.Unit
      Seq [e] -> trexp e
      Seq (e : es) -> trexp e *> trexp (Seq es)
      Array (Id typeid) len init ->
        checkInt len *> checkInt init *> find tenv typeid
          <&> ExpTy exp
      Record (Id typeId) fields -> do
        record <- find tenv typeId
        case record of
          T.Record _ _ -> return ()
          _ -> Left $ "the type " ++ typeId ++ " is not a record"
        let T.Record tyfields _ = record
        checkRecordField fields tyfields
        return $ ExpTy exp record
      LValue l -> trvar l
      FunCall (Id funname) args -> case M.lookup funname venv of
        Nothing -> Left $ "undefined function: " ++ funname
        Just (T.VarEntry _) -> Left $ "variable \"" ++ funname ++ "\" is not a function"
        Just (T.FunEntry argTypes retType) -> do
          transedArgs <- fmap ty <$> mapM trexp args
          if argTypes /= transedArgs
            then Left $ "type mismatch funcall of " ++ funname
            else return retType <&> ExpTy exp
      IfThen cond body -> do
        condExpTy <- trexp cond
        _ <- checkType T.Int $ ty condExpTy
        bodyExpTy <- trexp body
        _ <- checkType T.Unit $ ty bodyExpTy
        return . ExpTy exp $ ty bodyExpTy
      IfThenElse cond thenExp elseExp -> do
        condExpTy <- trexp cond
        _ <- checkType T.Int $ ty condExpTy
        thenExpTy <- trexp thenExp
        elseExpTy <- trexp elseExp
        _ <- checkType (ty thenExpTy) (ty elseExpTy)
        return . ExpTy exp $ ty thenExpTy
      WhileDo cond body -> do
        _ <- trexp cond >>= checkType T.Int . ty
        trexp body <&> ExpTy exp . ty
      Break -> Right $ ExpTy Break T.Unit
      ForToDo id s e body ->
        let whileLetExp =
              LetInEnd
                [VarDec (ShortVarDec id s)]
                ( WhileDo
                    (Less (LValue (Variable id)) e)
                    ( Seq
                        [ body,
                          Assign
                            (Variable id)
                            (Plus (LValue (Variable id)) (Int 1))
                        ]
                    )
                )
         in trexp whileLetExp
      Assign _ exp -> Right $ ExpTy exp T.Unit
      LetInEnd decs exp -> do
        (newVEnv, newTEnv) <- transDec venv tenv decs
        transExp newVEnv newTEnv exp
      Brack e -> trexp e
    --- trexp end
    checkDoubleArgsInt e1 e2 = checkInt e1 >> checkInt e2 <&> ExpTy exp
    checkType t1 t2 =
      if t1 == t2
        then Right t1
        else Left $ "type mismatch: expected " ++ show t1 ++ ", actual " ++ show t2
    checkInt e = case trexp e <&> ty of
      Right T.Int -> Right T.Int
      Left e -> Left e
      Right ty -> Left $ "type mismatch: expected Int, actual " ++ show ty
    trvar lvalue = case lvalue of
      Variable (Id name) -> case venv M.!? name of
        Just (T.VarEntry ty) -> Right $ ExpTy exp ty
        _ -> Left $ "undefined variable: " ++ name
      DotAccess v (Id name) -> checkRecord v name <&> ExpTy exp
      Index v n -> case trexp n >>= checkType T.Int . ty >> trvar v <&> ty of
        Right (T.Array ty _) -> Right $ ExpTy exp ty
        _ -> Left $ "undefined variable: " ++ show v
    checkRecord lv field = case trvar lv <&> ty of
      Right (T.Record recordTy _) -> case lookup field recordTy of
        Just ty -> Right ty
        _ -> Left $ "undefined field: " ++ field
      Right _ -> Left $ "Cannot access field because " ++ show lv ++ " is not a record"
      _ -> Left $ "undefined variable: " ++ field
    checkRecordField fields tyfields =
      mapM_
        ( \(Id name, exp) -> do
            exptype <- trexp exp <&> ty
            ty <- case lookup name tyfields of
              Nothing -> Left $ "undefined field: " ++ name
              Just ty -> Right ty
            if
                | ty == exptype -> Right ()
                | exptype == T.Nil -> Right ()
                | otherwise -> Left $ "field type mismatch expected: " ++ show ty ++ " but actual: " ++ show exptype
        )
        fields

transDec :: T.VEnv -> T.TEnv -> [Dec] -> Either String (T.VEnv, T.TEnv)
transDec v t decs = do
  let envs = insertHeader v t decs
  foldlM trDec envs decs
  where
    insertHeader venv tenv decs = case decs of
      [] -> (venv, tenv)
      dec : decs -> case dec of
        VarDec _ -> insertHeader venv tenv decs
        TyDec (Id name) _ ->
          insertHeader venv (M.insert name (T.Name Nothing) tenv) decs
        FunDec (ShortFunDec (Id name) _ _) -> insertHeader venv tenv decs
        FunDec (LongFunDec (Id name) _ _ _) -> insertHeader venv tenv decs

    trDec (venv, tenv) dec = case dec of
      TyDec (Id name) tydec -> case tenv M.!? name of
        Just (T.Name (Just ref)) -> Left "multiple definition of type"
        Just (T.Name Nothing) -> do
          reftype <- case tydec of
            Type (Id id) -> Right $ T.Name (Just id)
            RecordType _ -> transTy tenv tydec
            ArrayType _ -> transTy tenv tydec
          return (venv, M.insert name reftype tenv)
        Just _ -> do
          ty <- transTy tenv tydec
          return (venv, M.insert name ty tenv)
        _ -> undefined -- unreachable pattern
      VarDec vdec -> trvardec venv tenv vdec
      FunDec fdec@ShortFunDec {} -> trfundec venv tenv fdec
      FunDec fdec@(LongFunDec (Id name) tyfields (Id retType) _) -> do
        ty <- find tenv retType
        args <- tyListFromTyFields tenv tyfields
        trfundec (M.insert name (T.FunEntry args ty) venv) tenv fdec
    trvardec venv tenv vdec = case vdec of
      ShortVarDec (Id name) exp -> case transExp venv tenv exp of
        Right ExpTy {Semant.Semant.exp = _, ty = ty} -> Right (M.insert name (T.VarEntry ty) venv, tenv)
        Left e -> Left e
      LongVarDec (Id name) (Id typeId) exp -> case transExp venv tenv exp of
        Right ExpTy {Semant.Semant.exp = _, ty = inferedTy} -> do
          actualType <- find tenv typeId
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
        specifiedretType <- find tenv retTypeId
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

find :: T.TEnv -> [Char] -> Either [Char] T.Ty
find tenv name = find' tenv name []
  where
    find' tenv name searched
      | name `elem` searched = Left "cycle definition"
      | otherwise = case tenv M.!? name of
        Nothing -> Left $ "undefined type of \"" ++ name ++ "\""
        Just t@(T.Name ref) -> case ref of
          Just str -> find' tenv str $ name : searched
          Nothing -> Left $ "undefined type of \"" ++ name ++ "\""
        Just ty -> Right ty

transTy :: T.TEnv -> Type -> Either String T.Ty
transTy tenv (Type (Id name)) = case tenv M.!? name of
  Nothing -> Left $ "undefined type of: " ++ name
  Just t@(T.Name ref) -> case ref of
    Just str -> case tenv M.!? str of
      Just ty -> Right ty
      Nothing -> Left $ "undefined type of \"" ++ name ++ "\""
    Nothing -> Right t
  Just ty -> Right ty
transTy tenv (ArrayType name) = do
  ty <- transTy tenv (Type name)
  return $ T.Array ty 0
transTy tenv (RecordType tyfields) = do
  fields <- mapM mkSymTyAList tyfields
  return $ T.Record fields 0
  where
    mkSymTyAList (Id fieldName, typeId) = do
      ty <- transTy tenv (Type typeId)
      return (fieldName, ty)
