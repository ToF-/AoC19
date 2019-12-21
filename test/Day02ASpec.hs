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

