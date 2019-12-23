module Day03B 
    where
import Day03A

steps :: Position -> Position -> [Direction] -> Maybe Int
steps _ _ [] = Nothing
steps (x0,y0) (x1,y1) [R l] = Just (x1-x0)
