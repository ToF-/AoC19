module Day03B 
    where
import Day03A

steps :: Position -> Position -> [Direction] -> Maybe Int
steps _ _ [] = Nothing
steps (x0,y0) (x1,y1) [D l] | x0 == x1 && y1 >= (y0-l) = Just (y0-y1) 
                            | otherwise = Nothing
steps (x0,y0) (x1,y1) [L l] | y0 == y1 && x1 >= (x0-l) = Just (x0-x1)
                            | otherwise = Nothing
steps (x0,y0) (x1,y1) [U l,R 10] = (+) <$> Just l <*> steps (0,4) (5,4) [R 10]
steps (0,4) (5,4) [R 10] = Just 5
steps (x0,y0) (x1,y1) (U l:ds) | x0 == x1 && y1 <= (y0+l) = Just (y1-y0)
                               | otherwise = Nothing
steps (x0,y0) (x1,y1) (R l:ds) | y0 == y1 && x1 <= (x0+l) = Just (x1-x0)
                               | otherwise = (+) <$> Just l <*> steps (x0+l,y0) (x1,y1) ds
