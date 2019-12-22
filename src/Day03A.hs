module Day03A
    where

type Position = (Int,Int)
type Segment = (Position,Position)
data Direction = L Int | R Int | U Int | D Int 
    deriving (Eq,Show)

extend :: Position -> Direction -> Segment
extend (x,y) (R l) = ((x,y),(x+l,y))

