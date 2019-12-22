module Day03ASpec
    where
import Test.Hspec
import Day03A

spec = describe "manhattan distance of closest intersection" $ do
    describe "segment" $ do
        it "is created from a position, a direction and a length" $ do
            (0,0) `extend` R 75 `shouldBe` H 0 0 75
            (5,3) `extend` L 75 `shouldBe` H 3 (-70) 5
            (5,7) `extend` U 10 `shouldBe` V 5 7 17
            (5,7) `extend` D 10 `shouldBe` V 5 (-3) 7

        it "two horizontal lines on different y don't intersect" $ do
            H 0 0 10 `intersect` H 1 3 12 `shouldBe` []

        it "two horizontal lines on same y intersect if their coords are within range" $ do
            H 0 0 10 `intersect` H 0 (-4) 4  `shouldBe` [(0,0),(1,0),(2,0),(3,0),(4,0)] 
            H 3 2 8 `intersect` H 3 4 6  `shouldBe` [(4,3),(5,3),(6,3)]
