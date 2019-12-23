module Day03B 
    where
import Day03A

steps :: Position -> Position -> [Direction] -> Maybe Int
steps _ _ [] = Nothing
