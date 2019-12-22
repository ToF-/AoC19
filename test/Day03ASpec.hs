module Day03ASpec
    where
import Test.Hspec
import Day03A

spec = describe "manhattan distance of closest intersection" $ do
    describe "segment" $ do
        it "is created from a position, a direction and a length" $ do
            (0,0) `extend` R 75 `shouldBe` ((0,0),(75,0))
            (10,0) `extend` R 25 `shouldBe` ((10,0),(35,0))
            (10,0) `extend` L 25 `shouldBe` ((10,0),(-15,0))
            (320,100) `extend` U 17 `shouldBe` ((320,100),(320,83))
            (320,100) `extend` D 17 `shouldBe` ((320,100),(320,117))

    describe "list of directions" $ do
        it "create a list of segments" $ do
            segments [R 75,D 30,L 12] 
                `shouldBe` [((0,0),(75,0))
                           ,((75,0),(75,30))
                           ,((75,30),(63,30))]

