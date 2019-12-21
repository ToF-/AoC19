module Day02ASpec
    where
import Test.Hspec
import System.IO
import Day02A

spec = describe "Intcode program" $ do
    it "halt on opcode 99" $ do
        run [99] `shouldBe` [99]

    it "do an addition on indirect addresses" $ do
        run [1,5,6,7,99,42,17,0] `shouldBe` [1,5,6,7,99,42,17,42+17]
        run [1,5,6,7,99,4807,23,0] `shouldBe` [1,5,6,7,99,4807,23,4807+23]
        run [1,6,7,8,99,0,42,17,0] `shouldBe` [1,6,7,8,99,0,42,17,42+17]

    it "do an addition on address beyond size of  initial program" $ do
        run [1,5,6,10,99,42,17] `shouldBe` [1,5,6,10,99,42,17,0,0,0,59]

    it "do an addition on address before current position in the program" $ do
        run [1,5,6,0,99,42,17] `shouldBe` [59,5,6,0,99,42,17]

    it "do a multiplication" $ do
        run [2,5,6,7,99,42,17,0] `shouldBe` [2,5,6,7,99,42,17,42*17]

    it "chain operations" $ do
        run [1,9,10,3
            ,2,3,11,0
            ,99
            ,30,40,50]
             `shouldBe` [3500,9,10,70
                        ,2,3,11,0
                        ,99
                        ,30,40,50]

        run [1,0,0,0,99] `shouldBe` [2,0,0,0,99]
        run [2,3,0,3,99] `shouldBe` [2,3,0,6,99]
        run [2,4,4,5,99,0] `shouldBe` [2,4,4,5,99,9801] 
        run [1,1,1,4,99,5,6,0,99] `shouldBe` [30,1,1,4,2,5,6,0,99]
