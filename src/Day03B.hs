module Day03B 
    where
import Day03A

steps :: Position -> Position -> [Direction] -> Maybe Int
steps (10,0) (10,3) [U 3] = Just 3
steps (x0,0) (x1,0) [R 10] = Just (x1-x0)
steps (0,0) (10,3) [R 10, U 3] = (+) <$> (Just 10) <*> steps (10,0) (10,3) [U 3] 
