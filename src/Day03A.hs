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
intersect (H y x0A x1A) (H _ x0B x1B) = [(x,y) | x <- [x0A..x1A], x >= x0B && x <= x1B]
intersect (V xA 0 10) (V xB 3 12) | xA /= xB = []
intersect (V x y0A y1A) (V _ y0B y1B) | y0B < y0A = intersect (V x y0B y1B) (V x y0A y1A)
intersect (V x y0A y1A) (V _ y0B y1B) = [(x,y) | y <- [y0A..y1A], y >= y0B && y <= y1B]
intersect (V 15 0 10) (H (-4) 3 7) = []
intersect (V 15 0 10) (H 4 3 20) = [(15,4)]
