module Day03A
    where

type Position = (Int,Int)

data Segment = H Int Int Int 
             | V Int Int Int
    deriving (Eq,Show)
    
data Direction = L Int | R Int | U Int | D Int 
    deriving (Eq,Show)

extend :: Position -> Direction -> Segment
extend (x,y) (R l) = H y x (x+l)
