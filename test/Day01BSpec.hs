module Day01BSpec
    where
import Test.Hspec
import System.IO
import Day01B

spec = describe "fuelRequirement" $ do
    it "compute the fuel requirement of a module, including the fuel mass" $ do
        fuelRequirement 14 `shouldBe` 2
        fuelRequirement 1969 `shouldBe` 654 + 216 + 70 + 21 + 5

    it "compute the total fuel requirements for the puzzle" $ do
        handle <- openFile "input/Day1A.txt" ReadMode
        contents <- hGetContents handle
        let modules = map read $ lines contents
        sumFuelRequirements modules  `shouldBe` 5069241
    -- done
