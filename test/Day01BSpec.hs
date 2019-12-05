module Day01BSpec where

import Test.Hspec
import Day01B

spec = describe "requiredFuel" $ do
    it "Fuel itself requires fuel just like a module - take its mass, divide by three, round down, and subtract 2."  $ do
        requiredFuelComplete 2 `shouldBe` 0
        requiredFuelComplete 14 `shouldBe` 2
        requiredFuelComplete 100756 `shouldBe` 50346
