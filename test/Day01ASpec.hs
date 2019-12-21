module Day01ASpec
    where
import Test.Hspec

spec = describe "dummy test" $ do
    it "should fail then pass" $ do
        2+2 `shouldBe` 4
