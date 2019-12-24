module Day04BSpec
    where
import Test.Hspec
import Day04B

spec = describe "legitPassword" $ do
    it "is a 6 digit number" $ do
        legitPassword 4807 `shouldBe` False
        legitPassword 112233 `shouldBe` True
