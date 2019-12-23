module Day03ASpec
    where
import Test.Hspec
import Day03A

spec = describe "manhattan distance of closest intersection" $ do
    describe "segment" $ do
        it "is created from a position, a direction and a length" $ do
            (0,0) `extend` R 75 `shouldBe` H 0 0 75
            (5,3) `extend` L 75 `shouldBe` H 3 5 (-70) 
            (5,7) `extend` U 10 `shouldBe` V 5 7 17
            (5,7) `extend` D 10 `shouldBe` V 5 (-3) 7

        it "two horizontal lines on different y don't intersect" $ do
            H 0 0 10 `intersect` H 1 3 12 `shouldBe` []

        it "two horizontal lines on same y intersect if their coords are within range" $ do
            H 0 0 10 `intersect` H 0 (-4) 4  `shouldBe` [(0,0),(1,0),(2,0),(3,0),(4,0)] 
            H 3 2 8 `intersect` H 3 4 6  `shouldBe` [(4,3),(5,3),(6,3)]
            H 7 2 8 `intersect` H 7 (-40) (-20) `shouldBe` []

        it "two vertical lines on different x don't intersect" $ do
            V 0 0 10 `intersect` V 1 3 12 `shouldBe` []

        it "two vertical lines on same x intersect if their coords are within range" $ do
            V 0 0 10 `intersect` V 0 (-4) 4  `shouldBe` [(0,0),(0,1),(0,2),(0,3),(0,4)] 
            V 3 2 8 `intersect` V 3 4 6  `shouldBe` [(3,4),(3,5),(3,6)]
            V 7 2 8 `intersect` V 7 (-40) (-20) `shouldBe` []

        it "a vertical intersect with a horizontal depending on x and y" $ do
            V 15 0 10 `intersect` H (-4) 3 7 `shouldBe` []
            V 15 0 10 `intersect` H 4 3 20 `shouldBe` [(15,4)]
            H 5 0 10 `intersect` V 4 3 20 `shouldBe` [(4,5)]

    describe "path" $ do
        it "is created from a position and a set of directions and lengths" $ do
            path (0,0) [R 8, U 5, L 5, D 3] `shouldBe` 
                [H 0 0 8, V 8 0 5, H 5 3 8, V 3 2 5]
            path (10,20) [R 8, U 5, L 5, D 3] `shouldBe` 
                [H 20 10 18, V 18 20 25, H 25 13 18, V 13 22 25]
