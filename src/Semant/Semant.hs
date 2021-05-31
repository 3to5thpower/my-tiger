module Semant.Semant where

import Control.Monad (forM, liftM)
import Data.Functor ((<&>))
import qualified Data.Map as M
import Parse.Data
import qualified Semant.Types as T

data ExpTy = ExpTy {exp :: Exp, ty :: T.Ty} deriving (Show)

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
      Plus e1 e2 -> checkInt e1 >> checkInt e2 >> return T.Int <&> ExpTy exp
      Minus e1 e2 -> checkInt e1 >> checkInt e2 >> return T.Int <&> ExpTy exp
      Times e1 e2 -> checkInt e1 >> checkInt e2 >> return T.Int <&> ExpTy exp
      Div e1 e2 -> checkInt e1 >> checkInt e2 >> return T.Int <&> ExpTy exp
      Less e1 e2 -> checkInt e1 >> checkInt e2 >> return T.Int <&> ExpTy exp
      LessEqual e1 e2 -> checkInt e1 >> checkInt e2 >> return T.Int <&> ExpTy exp
      Greater e1 e2 -> checkInt e1 >> checkInt e2 >> return T.Int <&> ExpTy exp
      GreaterEqual e1 e2 -> checkInt e1 >> checkInt e2 >> return T.Int <&> ExpTy exp
      Equal e1 e2 -> checkInt e1 >> checkInt e2 >> return T.Int <&> ExpTy exp
      NotEqual e1 e2 -> checkInt e1 >> checkInt e2 >> return T.Int <&> ExpTy exp
      And e1 e2 -> checkInt e1 >> checkInt e2 >> return T.Int <&> ExpTy exp
      Or e1 e2 -> checkInt e1 >> checkInt e2 >> return T.Int <&> ExpTy exp
      Negate e -> checkInt e >> return T.Int <&> ExpTy exp
      --- other pure expression
      Seq (e : es) -> trexp e >> trexp (Seq es)
      Array typeid len init -> do
        elemTy <- transTy tenv (Type typeid)
        checkInt len >> checkInt init >> return elemTy <&> ExpTy exp
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

transDec :: VEnv -> TEnv -> Dec -> Either String (VEnv, TEnv)
transDec venv tenv dec = case dec of
  TyDec name ty -> Right (venv, M.insert (show name) (transTy tenv ty) tenv)
  VarDec vdec -> trvardec vdec
  where
    trvardec vdec = case vdec of
      ShortVarDec name exp -> case transExp venv tenv exp of
        Right ExpTy {Semant.Semant.exp = _, ty = ty} -> Right (M.insert (show name) (T.VarEntry ty) venv, tenv)
        Left e -> Left e
      LongVarDec name ty exp -> case transExp venv tenv exp of
        Right ExpTy {Semant.Semant.exp = _, ty = inferedTy} ->
          if transTy tenv ty == inferedTy
            then Right (M.insert (show name) (T.VarEntry ty) venv, tenv)
            else Left $ "Couldn't match expected type: " ++ show inferedTy ++ "actual: " ++ show ty

transTy :: TEnv -> Type -> Either String T.Ty
transTy tenv ty = Left ""
