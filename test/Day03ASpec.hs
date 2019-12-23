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
            (5,7) `extend` D 10 `shouldBe` V 5 7 (-3) 

        it "two horizontal lines on different y don't intersect" $ do
            H 0 0 10 `intersect` H 1 3 12 `shouldBe` []
            intersect (H 0 0 8) (H 5 5 2) `shouldBe` []

        it "two horizontal lines on same y intersect if their coords are within range" $ do
            H 0 0 10 `intersect` H 0 (-4) 4  `shouldBe` [(0,0),(1,0),(2,0),(3,0),(4,0)] 
            H 3 2 8 `intersect` H 3 4 6  `shouldBe` [(4,3),(5,3),(6,3)]
            H 7 2 8 `intersect` H 7 (-40) (-20) `shouldBe` []

        it "two vertical lines on different x don't intersect" $ do
            V 0 0 10 `intersect` V 1 3 12 `shouldBe` []
            intersect (V 0 0 8) (V 5 5 2) `shouldBe` []

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
                [H 0 0 8, V 8 0 5, H 5 8 3, V 3 5 2]
            path (10,20) [R 8, U 5, L 5, D 3] `shouldBe` 
                [H 20 10 18, V 18 20 25, H 25 18 13, V 13 25 22]

        it "can cross another path" $ do
            path (0,0) [R 8] `cross` path (5,5) [L 3]  `shouldBe` []
            path (0,0) [R 8] `cross` path (5,5) [D 13]  `shouldBe` [(5,0)]
            let p1 = path (0,0) [R 8, U 5, L 5, D 3]
            let p2 = path (0,0) [U 7, R 6, D 4, L 4]
            let crosses = p1 `cross` p2
            (3,3) `elem` crosses  `shouldBe` True
            (6,5) `elem` crosses  `shouldBe` True 

    describe "manhattan distance" $ do
        it "is the sum of the absolute x distance and y distance" $ do
            distance (0,0) (5,5) `shouldBe` 10
            distance (4,3) (1,1) `shouldBe` 5

    describe "distance from central port" $ do
        it "is Nothing if paths don't cross each other" $ do
            let p1 = path (0,0) [R 8]
                p2 = path (0,0) [U 5]
            distanceFrom (0,0) p1 p2  `shouldBe` Nothing

        it "is the manhattan distance of the closest intersection to (0,0) of two paths" $ do 
            let p1 = path (0,0) [R 8, U 5, L 5, D 3]
            let p2 = path (0,0) [U 7, R 6, D 4, L 4]
            distanceFrom (0,0) p1 p2 `shouldBe` Just 6

        it "passes puzzle examples" $ do
            let lineA = "R75,D30,R83,U83,L12,D49,R71,U7,L72"
                lineB = "U62,R66,U55,R34,D71,R55,D58,R83"
                pathA = path (0,0) $ readDirections lineA
                pathB = path (0,0) $ readDirections lineB
            distanceFrom (0,0) pathA pathB `shouldBe` Just 159

-- 
-- R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51
-- U98,R91,D20,R16,D67,R40,U7,R15,U6,R7 = distance 135

    describe "readDirections" $ do
        it "can read a path in the format of the puzzle" $ do
            readDirections "R75,D30,R83,L12" `shouldBe` [R 75, D 30, R 83, L 12]

        
