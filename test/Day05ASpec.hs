module Day05ASpec
    where
import Test.Hspec
import Control.Monad.Writer (Writer, writer, runWriter)
import System.IO
import Day05A

shouldResultIn :: [Code] -> [Code] -> Expectation
shouldResultIn code expected = 
        do
            result <- run (RW getLine putStrLn) code
            result `shouldBe` expected

spec = describe "an IntCode program" $ do
    it "halts on opcode 99" $ do
        [99] `shouldResultIn` [99]

    it "can do an addition on indirect addresses" $ do
        [1,5,6,7,99,42,17,0] `shouldResultIn` [1,5,6,7,99,42,17,42+17]
        [1,5,6,7,99,4807,23,0] `shouldResultIn` [1,5,6,7,99,4807,23,4807+23]
        [1,6,7,8,99,0,42,17,0] `shouldResultIn` [1,6,7,8,99,0,42,17,42+17]

    it "can do an addition on address beyond size of initial program" $ do
        [1,5,6,10,99,42,17] `shouldResultIn` [1,5,6,10,99,42,17,0,0,0,59]

    it "can do an addition on address before current position in the program" $ do
        [1,5,6,0,99,42,17] `shouldResultIn` [59,5,6,0,99,42,17]
 
    it "can do a multiplication" $ do
        [2,5,6,7,99,42,17,0] `shouldResultIn` [2,5,6,7,99,42,17,42*17]

    it "can chain instructions" $ do
        [1,9,10,3  ,2,3,11,0  ,99 ,30,40,50]
             `shouldResultIn` [3500,9,10,70
                              ,2,3,11,0
                              ,99
                              ,30,40,50]

    it "can do an input operation" $ do
        let input = return "42\n" --return "42\n" :: IO String
            output = putStrLn
            code = [3,3,99]
        result <- run (RW input output) code
        result  `shouldBe` [3,3,99,42]
