module Day04ASpec
    where
import Test.Hspec
import Day04A

spec = describe "legitPassword" $ do
    it "is a six digit number" $ do
        legitPassword 42 `shouldBe` False
        legitPassword 111111 `shouldBe` True
        legitPassword 1111111 `shouldBe` False
