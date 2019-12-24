module Day03BSpec
    where
import Test.Hspec
import Day03A
import Day03B
import System.IO

spec = describe "steps"  $ do
    describe "counts the steps on a path from a point to a point" $ do
        it "towards right" $ do
            steps (0,0) (3,3) [] `shouldBe` Nothing
            steps (0,0) (3,0) [R 10] `shouldBe` Just 3
            steps (0,0) (5,0) [R 10] `shouldBe` Just 5
            steps (3,0) (5,0) [R 10] `shouldBe` Just 2
            steps (0,0) (15,0) [R 10] `shouldBe` Nothing
            steps (0,0) (5,1) [R 10] `shouldBe` Nothing

        it "upward" $ do
            steps (0,0) (0,4) [U 5] `shouldBe` Just 4
            steps (0,2) (0,4) [U 5] `shouldBe` Just 2
            steps (0,0) (0,14) [U 5] `shouldBe` Nothing
            steps (0,0) (1,4) [U 5] `shouldBe` Nothing

        it "towards left" $ do
            steps (0,0) (-4,0) [L 5] `shouldBe` Just 4
            steps (-2,0) (-4,0) [L 5] `shouldBe` Just 2
            steps (0,0) (-14,0) [L 5] `shouldBe` Nothing
            steps (0,0) (-4,1) [L 5] `shouldBe` Nothing

        it "downward" $ do
            steps (0,0) (0,-4) [D 5]  `shouldBe` Just 4
            steps (0,1) (0,-4) [D 5]  `shouldBe` Just 5
            steps (0,0) (0,-14) [D 5]  `shouldBe` Nothing
            steps (0,0) (1,-4) [D 5]  `shouldBe` Nothing

        it "twice towards right" $ do
            steps (0,0) (6,0) [R 4, R 5] `shouldBe` Just 6

        it "up and then right" $ do
            steps (0,0) (4,5) [U 5, R 10] `shouldBe` Just 9

        it "left and then down" $ do
            steps (0,0) (-4,-3) [L 4, D 10] `shouldBe` Just 7

        it "down and then right" $ do
            steps (0,0) (7,-2) [D 2, R 10] `shouldBe` Just 9

        it "on directions given as example" $ do
--           ...........
--           .+-----+...
--           .|.....|...
--           .|..+--X-+.
--           .|..|..|.|.
--           .|.-X--+.|.
--           .|..|....|.
--           .|.......|.
--           .o-------+.
--           ...........
            let lineA = "R8,U5,L5,D3"
                lineB = "U7,R6,D4,L4"
                dirsA = readDirections lineA
                dirsB = readDirections lineB
            steps (0,0) (3,3) dirsA `shouldBe` Just 20
            steps (0,0) (3,3) dirsB `shouldBe` Just 20
            steps (0,0) (6,5) dirsA `shouldBe` Just 15
            steps (0,0) (6,5) dirsB `shouldBe` Just 15

    describe "minimalSteps" $ do
        it "gives the fewest combined steps for 2 paths to come to an intersection" $ do
            let lineA = "R8,U5,L5,D3"
                lineB = "U7,R6,D4,L4"
                dirsA = readDirections lineA
                dirsB = readDirections lineB
            minimalSteps (0,0) dirsA dirsB `shouldBe` Just 30 
        it "gives the fewest combined steps for the examples given" $ do
            let lineA = "R75,D30,R83,U83,L12,D49,R71,U7,L72"
                lineB = "U62,R66,U55,R34,D71,R55,D58,R83"
                dirsA = readDirections lineA
                dirsB = readDirections lineB
            minimalSteps (0,0) dirsA dirsB `shouldBe` Just 610
            let lineA = "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51"
                lineB = "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7"
                dirsA = readDirections lineA
                dirsB = readDirections lineB
            minimalSteps (0,0) dirsA dirsB `shouldBe` Just 410

        it "finds the fewest combined steps in the given puzzle" $ do
            handle <- openFile "input/Day3A.txt" ReadMode
            contents <- hGetContents handle
            let [lineA,lineB] = lines contents
                dirsA = readDirections lineA
                dirsB = readDirections lineB
            minimalSteps (0,0) dirsA dirsB `shouldBe` Just 12304
