module Day01ASpec
    where
import Test.Hspec
import Day01A

spec = describe "fuelRequirement" $ do
    it "comptute the fuel requirement of a module" $ do
        fuelRequirement 14 `shouldBe` 2
