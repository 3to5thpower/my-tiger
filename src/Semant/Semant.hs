{-# LANGUAGE MultiWayIf #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Semant.Semant where

import Control.Monad (when)
import Data.Functor ((<&>))
import qualified Data.Map as M
import Parse.Data
  ( Dec (VarDec),
    Exp (..),
    Id (Id),
    LValue (DotAccess, Index, Variable),
    VarDec (ShortVarDec),
  )
import Semant.AstUtil (isOpeExp)
import {-# SOURCE #-} Semant.TransDec (transDec)
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
      exp | isOpeExp exp -> trOpeExp exp
      Seq [] -> Right $ ExpTy exp T.Unit
      Seq [e] -> trexp e
      Seq (e : es) -> trexp e *> trexp (Seq es)
      Array (Id typeid) len init ->
        checkType T.Int len *> checkType T.Int init *> T.actualTy tenv typeid
          <&> ExpTy exp
      Record (Id typeId) fields -> do
        record <- T.actualTy tenv typeId
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
      IfThen cond body -> checkType T.Int cond >> checkType T.Unit body
      IfThenElse cond thenExp elseExp -> do
        checkType T.Int cond
        t <- trexp thenExp <&> ty
        e <- trexp elseExp <&> ty
        when (t /= e) $
          Left $
            "type mismatch: expected "
              ++ show t
              ++ " but actual "
              ++ show e
        return $ ExpTy exp e
      WhileDo cond body -> checkType T.Int cond >> trexp body
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
    trOpeExp exp = case exp of
      Plus e1 e2 -> checkType T.Int e1 >> checkType T.Int e2
      Minus e1 e2 -> checkType T.Int e1 >> checkType T.Int e2
      Times e1 e2 -> checkType T.Int e1 >> checkType T.Int e2
      Div e1 e2 -> checkType T.Int e1 >> checkType T.Int e2
      Less e1 e2 -> checkType T.Int e1 >> checkType T.Int e2
      LessEqual e1 e2 -> checkType T.Int e1 >> checkType T.Int e2
      Greater e1 e2 -> checkType T.Int e1 >> checkType T.Int e2
      GreaterEqual e1 e2 -> checkType T.Int e1 >> checkType T.Int e2
      Equal e1 e2 -> checkType T.Int e1 >> checkType T.Int e2
      NotEqual e1 e2 -> checkType T.Int e1 >> checkType T.Int e2
      And e1 e2 -> checkType T.Int e1 >> checkType T.Int e2
      Or e1 e2 -> checkType T.Int e1 >> checkType T.Int e2
      Negate e -> checkType T.Int e
      _ -> undefined -- unreachable
    checkType t exp = case trexp exp <&> ty of
      Right typ | typ == t -> Right $ ExpTy exp t
      Left e -> Left e
      Right ty -> Left $ "type mismatch: expected " ++ show t ++ ", actual " ++ show ty
    trvar lvalue = case lvalue of
      Variable (Id name) -> case venv M.!? name of
        Just (T.VarEntry ty) -> Right $ ExpTy exp ty
        _ -> Left $ "undefined variable: " ++ name
      DotAccess v (Id name) -> checkRecord v name <&> ExpTy exp
      Index v n -> case checkType T.Int n >> trvar v <&> ty of
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
