module Day02BSpec
    where
import Test.Hspec
import System.IO
import Day02B

spec = describe "findNounVerb" $ do
    it "entails setting up the program" $ do
        setup 42 17 [1,2,3,4,5] `shouldBe` [1,42,17,4,5]

    it "entails getting the result of the program" $ do
        result [42,15,34]  `shouldBe` 42

