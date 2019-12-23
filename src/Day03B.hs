module Day03B 
    where
import Day03A

steps :: Position -> Position -> [Direction] -> Int
steps (x0,0) (x1,0) [R 10] = x1-x0
steps (0,0) (10,3) [R 10, U 3] = 13
