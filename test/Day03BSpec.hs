module Day03BSpec
    where
import Test.Hspec
import Day03A
import Day03B
import System.IO

spec = describe "steps"  $ do
    it "counts the steps on a path from a point to a point" $ do
        steps (0,0) (3,0) [R 10] `shouldBe` Just 3 
        steps (2,0) (3,0) [R 10] `shouldBe` Just 1 
