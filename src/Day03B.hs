module Day03B 
    where
import Day03A

steps :: Position -> Position -> [Direction] -> Maybe Int
steps _ _ [] = Nothing
steps (10,0) (10,3) [U 7] = Just 3
steps (10,0) (10,3) [U 3] = Just 3
steps (x0,y0) (x1,y1) [R _] | y0 /= y1 = Nothing
steps (x0,0) (x1,0) [R l] = if (x0+l) >= x1 then Just (x1-x0) else Nothing
steps (0,0) (10,3) (R 10:ds) = (+) <$> (Just 10) <*> steps (10,0) (10,3) ds
