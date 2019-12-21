module Day02ASpec
    where
import Test.Hspec
import System.IO
import Day02A

spec = describe "Intcode program" $ do
    it "should halt on opcode 99" $ do
        run [99] `shouldBe` [99]

