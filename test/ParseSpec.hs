module ParseSpec where

import Parse
import Test.Hspec

spec :: Spec
spec = do
  describe "string \"hello\"" $ do
    it "Right(String \"hello\")" $ do
      parse "\"hello\"" `shouldBe` Right (String "\"hello\"")