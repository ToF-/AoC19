module Day02ASpec where

import Test.Hspec
import Day02A

spec = describe "computer" $ do
    it "Opcode 1 adds together numbers read from two positions and stores the result in a third position." $ do
        run [1,5,6,7,99,42,17,0] !! 7 `shouldBe` 59
    it "Opcode 2 works exactly like opcode 1, except it multiplies the two inputs instead of adding them." $ do
        
        run [2,5,6,7,99,42,17,0] !! 7 `shouldBe` 714

    it "can run whole programs" $ do
        run [1,9,10,3,2,3,11,0,99,30,40,50] 
            `shouldBe` [3500,9,10,70,2,3,11,0,99,30,40,50]
