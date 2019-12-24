module Day03B 
    where
import Day03A

steps :: Position -> Position -> [Direction] -> Maybe Int
steps _ _ [] = Nothing
steps (x0,y0) (x1,y1) (D l:ds) | x0 == x1 && y1 >= (y0-l) = Just (y0-y1) 
                               | otherwise = (+) <$> Just l <*> steps (x0,y0-l) (x1,y1) ds
steps (x0,y0) (x1,y1) (L l:ds) | y0 == y1 && x1 >= (x0-l) = Just (x0-x1)
                               | otherwise = (+) <$> Just l <*> steps (x0-l,y0) (x1,y1) ds
steps (x0,y0) (x1,y1) (U l:ds) | x0 == x1 && y1 <= (y0+l) = Just (y1-y0) 
                               | otherwise = (+) <$> Just l <*> steps (x0,y0+l) (x1,y1) ds
steps (x0,y0) (x1,y1) (R l:ds) | y0 == y1 && x1 <= (x0+l) = Just (x1-x0)
                               | otherwise = (+) <$> Just l <*> steps (x0+l,y0) (x1,y1) ds

minimalSteps :: Position -> [Direction] -> [Direction] -> Maybe Int
minimalSteps pos dirsA dirsB = minimum (filter (/= Just 0) allSteps)
    where
    allSteps = map combinedSteps crosses
    crosses = pathA `cross` pathB
    pathA = path (0,0) dirsA
    pathB = path (0,0) dirsB
    combinedSteps pos = (+) <$> steps (0,0) pos dirsA <*> steps (0,0) pos dirsB
