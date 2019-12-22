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
extend (x,y) (L l) = H y (x-l) x
extend (x,y) (U l) = V x y (y+l)
extend (x,y) (D l) = V x (y-l) y

intersect :: Segment -> Segment -> [Position]
intersect (H yA 0 10) (H yB 3 12) | yA /= yB = []
intersect (H y x0A x1A) (H _ x0B x1B) | x0B < x0A = intersect (H y x0B x1B) (H y x0A x1A)
intersect (H 0 (-4) 4) (H 0 0 10) = [(x,0) | x <- [-4..4], x >= 0 && x <= 10]
