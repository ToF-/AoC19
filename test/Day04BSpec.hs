module Day04BSpec
    where
import Test.Hspec
import Day04B

spec = describe "legitPassword" $ do
    it "as no group of two that is part of a larger group" $ do
        legitPassword 112233 `shouldBe` True
        legitPassword 113444 `shouldBe` False
        legitPassword 111111 `shouldBe` False

    describe "numberLegitPasswords" $ do
        it "tells how many legit passwords are in an interval" $ do
            numberLegitPasswords 111111 111133 `shouldBe` 0

