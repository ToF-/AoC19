{-# LANGUAGE FlexibleInstances #-}
module Day05ASpec
    where
import Test.Hspec
import Control.Monad.State
import System.IO
import Day05A


data MockLineIO = MockLineIO { setInput :: String, getOutput :: String }

instance LineIO (State MockLineIO) where
    input = do
        (MockLineIO s _) <- get
        return s

    output s = do
        (MockLineIO i _) <- get
        put (MockLineIO i s)

shouldResultIn :: [Code] -> [Code] -> Expectation
shouldResultIn code expected = 
        do
            result <- run code
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
        let code = [3,3,99]
            mock = MockLineIO { setInput = "42\n", getOutput = "" }
            st   = evalState (run code) mock
        st `shouldBe` [3,3,99,42]

    it "can do an output operation" $ do
        let code = [4,3,99,4807]
            mock = MockLineIO { setInput = "42\n", getOutput = "" }
            st   = execState (run code) mock
        getOutput st  `shouldBe` "4807"
    it "can copy its input into its output" $ do
        let code = [3,0,4,0,99]
            mock = MockLineIO { setInput = "17\n", getOutput = "" }
            st   = execState (run code) mock
        getOutput st  `shouldBe` "17"

    it "has immediate mode for first parameter" $ do
        [101,42,5,6,99,17] `shouldResultIn` [101,42,5,6,99,17,59]

    it "has immediate mode for second parameter" $ do
        [1001,5,23,6,99,17] `shouldResultIn` [1001,5,23,6,99,17,40]
