module Day04ASpec
    where
import Test.Hspec
import Day04A

spec = describe "legitPassword" $ do
    it "is a six digit number" $ do
        legitPassword 42 `shouldBe` False
        legitPassword 111111 `shouldBe` True
        legitPassword 1111111 `shouldBe` False

    it "contains at least 2 adjacent identical digits" $ do
        legitPassword 123456 `shouldBe` False
        legitPassword 123466 `shouldBe` True
        legitPassword 123446 `shouldBe` True
