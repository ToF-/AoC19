module Day03A
    where

type Position = (Int,Int)
type Segment = (Position,Position)
data Direction = L Int | R Int | U Int | D Int 
    deriving (Eq,Show)

extend :: Position -> Direction -> Segment
extend (x,y) (R l) = ((x,y),(x+l,y))
extend (x,y) (L l) = ((x,y),(x-l,y))
extend (x,y) (U l) = ((x,y),(x,y-l))
extend (x,y) (D l) = ((x,y),(x,y+l))

segments :: [Direction] -> [Segment]
segments = segmentAt (0,0)
    where
    segmentAt _ [] = []
    segmentAt p (d:ds) = segment : segmentAt (dest segment) ds
        where
        segment = (p `extend` d)
        dest = snd

