module Day03B 
    where
import Day03A

steps :: Position -> Position -> [Direction] -> Maybe Int
steps (x0,0) (x1,0) [R 10] = Just (x1-x0)
