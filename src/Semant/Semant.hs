module Semant.Semant where

import Control.Monad (forM, liftM)
import Data.Functor ((<&>), ($>))
import qualified Data.Map as M
import Parse.Data
import qualified Semant.Types as T

data ExpTy = ExpTy {exp :: Exp, ty :: T.Ty} deriving (Show, Eq)

semant :: Either String Exp -> Either String Exp
semant (Left s) = Left s
semant (Right e) = Right e

semantSimpleExp :: Exp -> T.Ty
semantSimpleExp e = case e of
  String s -> T.String
  Int n -> T.Int
  Nil -> T.Nil
  Unit -> T.Unit
  Negate e -> semantSimpleExp e

type VEnv = M.Map T.Symbol T.EnvEntry

type TEnv = M.Map T.Symbol T.Ty

-- transVar :: VEnv -> TEnv -> LValue -> Either String ExpTy
-- transVar venv tenv lvalue =

transExp :: VEnv -> TEnv -> Exp -> Either String ExpTy
transExp venv tenv exp = trexp exp
  where
    trexp exp = case exp of
      --- atom
      Nil -> Right $ ExpTy Nil T.Nil
      Unit -> Right $ ExpTy Unit T.Unit
      n@(Int x) -> Right $ ExpTy n T.Int
      s@(String str) -> Right $ ExpTy s T.String
      --- operator for int
      Plus e1 e2 -> checkInt e1 *> checkInt e2 $> T.Int <&> ExpTy exp
      Minus e1 e2 -> checkInt e1 *> checkInt e2 $> T.Int <&> ExpTy exp
      Times e1 e2 -> checkInt e1 *> checkInt e2 $> T.Int <&> ExpTy exp
      Div e1 e2 -> checkInt e1 *> checkInt e2 $> T.Int <&> ExpTy exp
      Less e1 e2 -> checkInt e1 *> checkInt e2 $> T.Int <&> ExpTy exp
      LessEqual e1 e2 -> checkInt e1 *> checkInt e2 $> T.Int <&> ExpTy exp
      Greater e1 e2 -> checkInt e1 *> checkInt e2 $> T.Int <&> ExpTy exp
      GreaterEqual e1 e2 -> checkInt e1 *> checkInt e2 $> T.Int <&> ExpTy exp
      Equal e1 e2 -> checkInt e1 *> checkInt e2 $> T.Int <&> ExpTy exp
      NotEqual e1 e2 -> checkInt e1 *> checkInt e2 $> T.Int <&> ExpTy exp
      And e1 e2 -> checkInt e1 *> checkInt e2 $> T.Int <&> ExpTy exp
      Or e1 e2 -> checkInt e1 *> checkInt e2 $> T.Int <&> ExpTy exp
      Negate e -> checkInt e >> return T.Int <&> ExpTy exp
      --- other pure expression
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
        checkType T.Int $ ty condExpTy
        bodyExpTy <- trexp body
        checkType T.Unit $ ty bodyExpTy
        return . ExpTy exp $ ty bodyExpTy
      IfThenElse cond thenExp elseExp -> do
        condExpTy <- trexp cond
        checkType T.Int $ ty condExpTy
        thenExpTy <- trexp thenExp
        elseExpTy <- trexp elseExp
        checkType (ty thenExpTy) (ty elseExpTy)
        return . ExpTy exp $ ty thenExpTy
      WhileDo cond body -> do
        condExpTy <- trexp cond
        checkType T.Int $ ty condExpTy
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
      Assign lvalue exp -> Right $ ExpTy exp T.Unit
      LetInEnd decs exp -> do
        (newVEnv, newTEnv) <- transDec venv tenv decs
        transExp newVEnv newTEnv exp
    --- trexp end
    checkType t1 t2 =
      if t1 == t2
        then Right t1
        else Left $ "type mismatch: expected t1, actual " ++ show t2
    checkUnit e = case e of
      Unit -> Right T.Unit
      e -> Left $ "type mismatch: expected (), actual " ++ show e
    checkInt e = case e of
      Int _ -> Right T.Int
      e -> Left $ "type mismatch: expected Int, actual " ++ show e
    trvar lvalue = case lvalue of
      Variable name -> case venv M.!? show name of
        Just (T.VarEntry ty) -> Right $ ExpTy exp ty
        _ -> Left $ "undefined variable: " ++ show name
      DotAccess v _ -> checkRecord v <&> ExpTy exp
      Index v n -> case checkInt n >> trvar v <&> ty of
        Right (T.Array ty _) -> Right $ ExpTy exp ty
        _ -> Left $ "undefined variable: " ++ show v
    checkRecord (DotAccess lv field) = case checkRecord lv of
      Right (T.Record recordTy _) -> case lookup (show field) recordTy of
        Just ty -> Right ty
        _ -> Left $ "undefined field: " ++ show field
      Right e -> Right e
      _ -> Left $ "undefined variable: " ++ show field
    checkRecord v = trvar v <&> ty
    checkRecordField :: [(Id, Exp)] -> [(String, T.Ty)] -> Either String ()
    checkRecordField fields tyfields = mapM_ f fields
      where
        f :: (Id, Exp) -> Either String ()
        f (Id name, exp) = do
          exptype <- trexp exp <&> ty
          ty <- case lookup name tyfields of
            Nothing -> Left $ "undefined field: " ++ name
            Just ty -> Right ty
          if ty == exptype
            then Right ()
            else Left $ "field type mismatch expected: " ++ show ty ++ " but actual: " ++ show exptype


transDec :: VEnv -> TEnv -> [Dec] -> Either String (VEnv, TEnv)
transDec v t [] = Right (v, t)
transDec v t (dec:decs) = do
  (venv, tenv) <- trDec v t dec
  transDec venv tenv decs
  where
    trDec:: VEnv -> TEnv -> Dec -> Either String (VEnv, TEnv)
    trDec venv tenv dec = case dec of
      TyDec (Id name) ty -> do
        ty <- transTy tenv ty
        return (venv, M.insert name ty tenv)
      VarDec vdec -> trvardec venv tenv vdec
    trvardec :: VEnv -> TEnv -> VarDec -> Either String (VEnv, TEnv)
    trvardec venv tenv vdec = case vdec of
      ShortVarDec name exp -> case transExp venv tenv exp of
        Right ExpTy {Semant.Semant.exp = _, ty = ty} -> Right (M.insert (show name) (T.VarEntry ty) venv, tenv)
        Left e -> Left e
      LongVarDec name (Id typeId) exp -> case transExp venv tenv exp of
        Right ExpTy {Semant.Semant.exp = _, ty = inferedTy} -> do
          actualType <- find tenv typeId
          if actualType == inferedTy
            then Right (M.insert (show name) (T.VarEntry actualType) venv, tenv)
            else Left $ "Couldn't match expected type: " ++ show inferedTy ++ "but actual: " ++ typeId
    trfundec :: VEnv -> TEnv -> FunDec -> Either String (VEnv, TEnv)
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
          then Left $ "Couldn't match expected type: " 
            ++ show retType ++ " but actual: " ++ retTypeId
          else return (M.insert name (T.FunEntry args retType) venv, tenv)

find :: TEnv -> T.Symbol -> Either String T.Ty
find tenv name = case tenv M.!? name of
  Nothing -> Left $ "undefined type of \"" ++ name
  Just ty -> Right ty

insertArgsToEnv :: VEnv -> TEnv -> TyFields -> Either String VEnv
insertArgsToEnv venv tenv [] = Right venv
insertArgsToEnv venv tenv (x:xs) = do
  insertedvenv <- insertArgToEnv tenv venv x
  insertArgsToEnv insertedvenv tenv xs
  where
    insertArgToEnv :: TEnv -> VEnv -> (Id, Id) -> Either String VEnv
    insertArgToEnv tenv venv (Id varId, Id typeId) =  case tenv M.!? typeId of
      Nothing -> Left $ "undefined type: \"" ++ typeId ++ "\" of variable \"" ++ varId ++ "\""
      Just ty -> Right $ M.insert varId (T.VarEntry  ty) venv 

tyListFromTyFields :: TEnv -> TyFields -> Either String [T.Ty]
tyListFromTyFields tenv = mapM (idToTy tenv)
  where 
    idToTy :: TEnv -> (Id, Id) -> Either String T.Ty
    idToTy tenv (Id varId, Id typeId) = case tenv M.!? typeId of
      Nothing -> Left $ "undefined type: \"" ++ typeId ++ " of variable \"" ++ varId ++ "\""
      Just ty -> Right ty

transTy :: TEnv -> Type -> Either String T.Ty
transTy tenv (Type (Id name)) = find tenv name
transTy tenv (ArrayType (Id name)) = do
  ty <- find tenv name
  return $ T.Array ty 0
transTy tenv (RecordType tyfields) = do
  fields <- mapM f tyfields
  return $ T.Record fields 0
  where
    f (Id fieldName, Id typeId) = do
      ty <- find tenv typeId
      return (fieldName, ty)

