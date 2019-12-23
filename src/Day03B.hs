module Day03B 
    where
import Day03A

steps :: Position -> Position -> [Direction] -> Maybe Int
steps (0,0) (3,0) [R 10] = Just 3
