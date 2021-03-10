module ParseSpec where

import Parse.Parser
import Test.Hspec

spec :: Spec
spec = do
  describe "Exp" $ do
    it "string" $ do
      parse "\"hello\"" `shouldBe` Right (String "hello")
    it "int" $ do
      parse "42" `shouldBe` Right (Int 42)
    it "nil" $ do
      parse "nil" `shouldBe` Right Nil
    it "Seq" $ do
      parse "(42;42;42)" `shouldBe` Right (Seq [Int 42, Int 42, Int 42])
    it "Unit" $ do
      parse "()" `shouldBe` Right Unit
    it "funcall without arguments" $ do
      parse "func()" `shouldBe` Right (FunCall (Id "func") [])
    it "funcall with arguments" $ do
      -- function arguments list is reversed
      parse "func(x, 2)"
        `shouldBe` Right (FunCall (Id "func") [Int 2, LValue (Variable (Id "x"))])
    it "Negate" $ do
      parse "-2" `shouldBe` Right (Negate (Int 2))
    it "Plus" $ do
      parse "1 + 2" `shouldBe` Right (Plus (Int 1) (Int 2))
    it "Minus" $ do
      parse "1 - 2" `shouldBe` Right (Minus (Int 1) (Int 2))
    it "Times" $ do
      parse "1 * 2" `shouldBe` Right (Times (Int 1) (Int 2))
    it "Div" $ do
      parse "1 / 2" `shouldBe` Right (Div (Int 1) (Int 2))
    it "Less" $ do
      parse "1 < 2" `shouldBe` Right (Less (Int 1) (Int 2))
    it "LessEqual" $ do
      parse "1 <= 2" `shouldBe` Right (LessEqual (Int 1) (Int 2))
    it "Greater" $ do
      parse "1 > 2" `shouldBe` Right (Greater (Int 1) (Int 2))
    it "GreaterEqual" $ do
      parse "1 >= 2" `shouldBe` Right (GreaterEqual (Int 1) (Int 2))
    it "Equal" $ do
      parse "1 = 2" `shouldBe` Right (Equal (Int 1) (Int 2))
    it "NotEqual" $ do
      parse "1 <> 2" `shouldBe` Right (NotEqual (Int 1) (Int 2))
    it "And" $ do
      parse "0 & 1" `shouldBe` Right (And (Int 0) (Int 1))
    it "Or" $ do
      parse "0 | 1" `shouldBe` Right (Or (Int 0) (Int 1))
    it "Record" $ do
      parse "Student { id=1, name =\"hoge\" }"
        `shouldBe` Right (Record (TypeId "Student") [(Id "name", String "hoge"), (Id "id", Int 1)])
    it "Array" $ do
      parse "intArray[10] of 0" `shouldBe` Right (Array (TypeId "int") (Int 10) (Int 0))