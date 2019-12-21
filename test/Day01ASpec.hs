module Day01ASpec
    where
import Test.Hspec
import System.IO
import Day01A

spec = describe "fuelRequirement" $ do
    it "compute the fuel requirement of a module" $ do
        fuelRequirement 14 `shouldBe` 2
        fuelRequirement 1969 `shouldBe` 654

    it "compute the total fuel requirement of a list of modules" $ do
        sumFuelRequirements [14,1969] `shouldBe` 2+654

    it "compute the total fuel requirements for the puzzle" $ do
        handle <- openFile "input/Day1A.txt" ReadMode
        contents <- hGetContents handle
        let modules = map read $ lines contents
        sumFuelRequirements modules  `shouldBe` 3381405
    -- done
        

