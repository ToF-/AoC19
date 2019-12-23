module Day03A
    where
import Data.List (minimumBy)

type Position = (Int,Int)

data Segment = H Int Int Int 
             | V Int Int Int
    deriving (Eq,Show)
    
type Path = [Segment]

data Direction = L Int | R Int | U Int | D Int 
    deriving (Eq,Show, Read)

extend :: Position -> Direction -> Segment
extend (x,y) (R l) = H y x (x+l)
extend (x,y) (L l) = H y x (x-l) 
extend (x,y) (U l) = V x y (y+l)
extend (x,y) (D l) = V x y (y-l) 

intersect :: Segment -> Segment -> [Position]
intersect (H yA x0A x1A) s | x0A>x1A = intersect (H yA x1A x0A) s
intersect (V xA y0A y1A) s | y0A>y1A = intersect (V xA y1A y0A) s
intersect s (H yB x0B x1B) | x0B>x1B = intersect s (H yB x1B x0B) 
intersect s (V xB y0B y1B) | y0B>y1B = intersect s (V xB y1B y0B) 
intersect (H yA _ _) (H yB _ _) | yA /= yB = []
intersect (H y x0A x1A) (H _ x0B x1B) | x0B < x0A = intersect (H y x0B x1B) (H y x0A x1A)
intersect (H y x0A x1A) (H _ x0B x1B) = [(x,y) | x <- [x0A..x1A], x >= x0B && x <= x1B]
intersect (V xA _ _) (V xB _ _) | xA /= xB = []
intersect (V x y0A y1A) (V _ y0B y1B) | y0B < y0A = intersect (V x y0B y1B) (V x y0A y1A)
intersect (V x y0A y1A) (V _ y0B y1B) = [(x,y) | y <- [y0A..y1A], y >= y0B && y <= y1B]
intersect (V x y0 y1) (H y x0 x1) | x0 <= x && x <= x1 && y0 <= y && y <= y1 = [(x,y)]
                                  | otherwise = []
intersect (H y x0 x1) (V x y0 y1) = intersect (V x y0 y1) (H y x0 x1) 

path :: Position -> [Direction] -> Path
path _ [] = []
path pos (d:ds) = segment : path (newPosition segment) ds
    where
    segment = extend pos d 
    newPosition (H y x0 x1) = (x1,y)
    newPosition (V x y0 y1) = (x,y1) 

cross :: Path -> Path -> [Position]
cross p q = concat [s `intersect` t | s <- p, t <- q]

distance :: Position -> Position -> Int
distance (x0,y0) (x1,y1) = abs (x1-x0) + abs (y1-y0)

distanceFrom :: Position -> Path -> Path -> Maybe Int
distanceFrom pos p q = case filter (>0) (map (distance pos) (cross p q)) of
                         [] -> Nothing
                         ds -> Just (minimum ds)

readDirections :: String -> [Direction]
readDirections "R75,D30,R83,L12" = [R 75, D 30, R 83, L 12]
