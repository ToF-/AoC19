module Day04BSpec
    where
import Test.Hspec
import Day04B

spec = describe "legitPassword" $ do
    it "is a 6 digit number" $ do
        legitPassword 4807 `shouldBe` False
        legitPassword 112233 `shouldBe` True

    it "has two adjacent digits that are identical" $ do
        legitPassword 123456 `shouldBe` False
        legitPassword 123455 `shouldBe` True
        legitPassword 123457 `shouldBe` False
        legitPassword 123445 `shouldBe` True

    it "has digits that never decrease from left to right" $ do
        legitPassword 223450 `shouldBe` False

    it "has two adjacent digits that are not part of a group of three" $ do
        legitPassword 123444 `shouldBe` False

    describe "digits" $ do
        it "gives a number's digit" $ do
            digits 1 4807  `shouldBe` [7]
            digits 2 4807  `shouldBe` [7,0]
