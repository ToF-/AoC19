module Day03ASpec
    where
import Test.Hspec
import Day03A

spec = describe "manhattan distance of closest intersection" $ do
    describe "segment" $ do
        it "is created from a position, a direction and a length" $ do
            (0,0) `extend` R 75 `shouldBe` H 0 0 75
