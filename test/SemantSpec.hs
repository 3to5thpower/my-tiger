{-# OPTIONS_GHC -Wno-name-shadowing #-}

module SemantSpec where

import qualified Data.Map as M
import Parse.Data
import Semant.Semant
import qualified Semant.Types as T
import Test.Hspec

baseDataEnv :: M.Map T.Symbol T.EnvEntry
baseDataEnv =
  M.union T.baseDataEnv $
    M.fromList
      [ ("foo", T.VarEntry T.Int),
        ("bar", T.VarEntry $ T.Array T.Int 0),
        ("bal", T.VarEntry $ T.Record [("name", T.String), ("id", T.Int)] 0)
      ]

baseTypesEnv :: M.Map T.Symbol T.Ty
baseTypesEnv =
  M.union T.baseTypesEnv $
    M.fromList
      [ ("intArray", T.Array T.Int 0),
        ("Student", T.Record [("name", T.String), ("id", T.Int)] 0)
      ]

trexp :: Exp -> Either String ExpTy
trexp = transExp baseDataEnv baseTypesEnv

check :: Exp -> T.Ty -> Expectation
check exp ty = fmap Semant.Semant.ty (trexp exp) `shouldBe` Right ty

spec :: Spec
spec = do
  describe "atom" $ do
    it "nil" $ do
      check Nil T.Nil
    it "unit" $ do
      check Unit T.Unit
    it "string" $ do
      check (String "hello") T.String
    it "int" $ do
      check (Int 42) T.Int
  describe "operator for int" $ do
    it "plus" $ do
      check (Plus (Int 1) (Int 2)) T.Int
    it "minus" $ do
      check (Minus (Int 1) (Int 2)) T.Int
    it "times" $ do
      check (Times (Int 1) (Int 2)) T.Int
    it "div" $ do
      check (Div (Int 2) (Int 1)) T.Int
    it "less" $ do
      check (Less (Int 1) (Int 2)) T.Int
    it "lessEqual" $ do
      check (LessEqual (Int 1) (Int 2)) T.Int
    it "greater" $ do
      check (Greater (Int 1) (Int 2)) T.Int
    it "greaterEqual" $ do
      check (GreaterEqual (Int 1) (Int 2)) T.Int
    it "equal" $ do
      check (Equal (Int 1) (Int 2)) T.Int
    it "not equal" $ do
      check (NotEqual (Int 1) (Int 2)) T.Int
    it "and" $ do
      check (And (Int 1) (Int 2)) T.Int
    it "or" $ do
      check (Or (Int 1) (Int 2)) T.Int
    it "negate" $ do
      check (Negate (Int 1)) T.Int
  describe "expression like statements" $ do
    it "sequence" $ do
      check (Seq [Int 1, String "hoge"]) T.String
    it "if-then" $ do
      check (IfThen (Int 1) Unit) T.Unit
    it "if-then-else" $ do
      check (IfThenElse (Int 1) (String "true") (String "false")) T.String
    it "while-do" $ do
      check (WhileDo (Int 1) (String "true")) T.String
    it "break" $ do
      check Break T.Unit
    it "for" $ do
      check (ForToDo (Id "i") (Int 0) (Int 10) (LValue (Variable (Id "i")))) T.Unit
    it "assign" $ do
      check (Assign (Variable (Id "x")) (Int 42)) T.Unit
  describe "other expressions" $ do
    it "array" $ do
      check (Array (Id "intArray") (Int 5) (Int 0)) $ T.Array T.Int 0
    it "record" $ do
      check (Record (Id "Student") [(Id "name", String "taro"), (Id "id", Int 1)]) $
        T.Record [("name", T.String), ("id", T.Int)] 0
    it "funcall" $ do
      check (FunCall (Id "substr") [String "hoge", Int 0, Int 2]) T.String
  describe "lvalue" $ do
    it "variable" $ do
      check (LValue (Variable (Id "foo"))) T.Int
    it "array variable" $ do
      check (LValue (Variable (Id "bar"))) $ T.Array T.Int 0
    it "array access" $ do
      check (LValue (Index (Variable (Id "bar")) (Int 0))) T.Int
    it "record variable" $ do
      check (LValue (Variable (Id "bal"))) $ T.Record [("name", T.String), ("id", T.Int)] 0
    it "record access" $ do
      check (LValue (DotAccess (Variable (Id "bal")) (Id "name"))) T.String
      check (LValue (DotAccess (Variable (Id "bal")) (Id "id"))) T.Int
  describe "let" $ do
    it "short variable declaration" $ do
      check (LetInEnd [VarDec (ShortVarDec (Id "foo") (String "hogehoge"))] $ LValue (Variable (Id "foo"))) T.String
    it "long variable declaration" $ do
      check (LetInEnd [VarDec (LongVarDec (Id "foo") (Id "string") (String "hogehoge"))] $ LValue (Variable (Id "foo"))) T.String
    it "short function declaration" $ do
      check (LetInEnd [FunDec (ShortFunDec (Id "f") [] (Int 1))] (FunCall (Id "f") [])) T.Int
    it "long function declaration" $ do
      check
        ( LetInEnd
            [ FunDec
                ( LongFunDec
                    (Id "f")
                    [(Id "x", Id "int"), (Id "y", Id "int")]
                    (Id "int")
                    (Int 1)
                )
            ]
            (FunCall (Id "f") [Int 1, Int 2])
        )
        T.Int
    it "normal type Declaration" $ do
      check (LetInEnd [TyDec (Id "myInt") (Type (Id "int")), VarDec (LongVarDec (Id "x") (Id "myInt") (Int 1))] (LValue (Variable (Id "x")))) T.Int
    it "array type Declaration" $ do
      check (LetInEnd [TyDec (Id "intArray") (ArrayType (Id "int"))] (Array (Id "intArray") (Int 10) (Int 0))) $
        T.Array T.Int 0
    it "record type Declaration" $ do
      check (LetInEnd [TyDec (Id "student") (RecordType [(Id "id", Id "int"), (Id "name", Id "string")])] (Record (Id "Student") [(Id "id", Int 1), (Id "name", String "hoge")])) $
        T.Record [("name", T.String), ("id", T.Int)] 0
    it "recursive function declaration" $ do
      check
        ( LetInEnd
            [ FunDec
                ( LongFunDec
                    (Id "f")
                    [(Id "x", Id "int")]
                    (Id "int")
                    ( IfThenElse
                        (Less (LValue (Variable (Id "x"))) (Int 2))
                        (Int 1)
                        ( Times
                            (LValue (Variable (Id "x")))
                            ( FunCall
                                (Id "f")
                                [Minus (LValue (Variable (Id "x"))) (Int 1)]
                            )
                        )
                    )
                )
            ]
            (FunCall (Id "f") [Int 5])
        )
        T.Int