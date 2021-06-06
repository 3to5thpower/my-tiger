module SemantSpec where

import Parse.Data
import Semant.Semant
import qualified Semant.Types as T
import Test.Hspec

trexp :: Exp -> Either String ExpTy
trexp = transExp T.baseDataEnv T.baseTypesEnv

check :: Exp -> T.Ty -> Expectation
check exp ty = trexp exp `shouldBe` Right (ExpTy exp ty)

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
        
