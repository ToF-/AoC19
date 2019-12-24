module Day04BSpec
    where
import Test.Hspec
import Day04B

spec = describe "legitPassword" $ do
    it "as no group of two that is part of a larger group" $ do
        legitPassword 112233 `shouldBe` True
        legitPassword 113444 `shouldBe` False

