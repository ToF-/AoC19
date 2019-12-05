module Day01ASpec where

import Test.Hspec
import Day01A

spec = describe "requiredFuel" $ do
    it "specifically, to find the fuel required for a module, take its mass, divide by three, round down, and subtract 2" $ do
        requiredFuel 12 `shouldBe` 2
        requiredFuel 100756 `shouldBe` 33583
