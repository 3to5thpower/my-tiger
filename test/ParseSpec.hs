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
    it "If Then" $ do
      parse "if 1 then 1" `shouldBe` Right (IfThen (Int 1) (Int 1))
    it "If Then Else" $ do
      parse "if 0 then 1 else 2" `shouldBe` Right (IfThenElse (Int 0) (Int 1) (Int 2))
    it "While" $ do
      parse "while 1 do 2" `shouldBe` Right (WhileDo (Int 1) (Int 2))
    it "for" $ do
      parse "for i:=0 to 10 do i" `shouldBe` Right (ForToDo (Id "i") (Int 0) (Int 10) (LValue (Variable (Id "i"))))
    it "break" $ do
      parse "while 1 do break" `shouldBe` Right (WhileDo (Int 1) Break)
    it "bracket" $ do
      parse "(42)" `shouldBe` Right (Brack (Int 42))

  describe "LValue" $ do
    it "variable" $ do
      parse "x" `shouldBe` Right (LValue (Variable (Id "x")))
    it "property" $ do
      parse "x.id" `shouldBe` Right (LValue (DotAccess (Variable (Id "x")) (Id "id")))
    it "index" $ do
      parse "x[5]" `shouldBe` Right (LValue (Index (Variable (Id "x")) (Int 5)))
  describe "Decs" $ do
    it "type dec" $ do
      parse "let type myInt = int in 1 end"
        `shouldBe` Right (LetInEnd [TyDec (TypeId "myInt") (Type (TypeId "int"))] (Int 1))
    it "var without type" $ do
      parse "let var x := 1 in x end" `shouldBe` Right (LetInEnd [VarDec (ShortVarDec (Id "x") (Int 1))] (LValue (Variable (Id "x"))))
    it "var with type" $ do
      parse "let var x : int := 1 in x end" `shouldBe` Right (LetInEnd [VarDec (LongVarDec (Id "x") (TypeId "int") (Int 1))] (LValue (Variable (Id "x"))))
    it "function without type" $ do
      parse "let function f() = 1 in 1 end" `shouldBe` Right (LetInEnd [FunDec (ShortFunDec (Id "f") [] (Int 1))] (Int 1))
    it "function with type" $ do
      parse "let function f(x:int,y:int):int = 1 in 1 end"
        `shouldBe` Right
          ( LetInEnd
              [ FunDec
                  ( LongFunDec
                      (Id "f")
                      [(Id "y", TypeId "int"), (Id "x", TypeId "int")]
                      (TypeId "int")
                      (Int 1)
                  )
              ]
              (Int 1)
          )