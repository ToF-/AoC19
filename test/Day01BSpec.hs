module Day01BSpec
    where
import Test.Hspec
import System.IO
import Day01B

spec = describe "fuelRequirement" $ do
    it "compute the fuel requirement of a module, including the fuel mass" $ do
        fuelRequirement 14 `shouldBe` 2
        fuelRequirement 1969 `shouldBe` 654 + 216 + 70 + 21 + 5
