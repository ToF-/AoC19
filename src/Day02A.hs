module Day02A where
import Data.Map as M
import Data.List as L

type Program = Map Int OpCode
type OpCode  = Int
type Position= Int

run :: [Int] -> [Int] 
run code = toCode program'
    where
    fromCode code = fromList (zip [0..] code)
    toCode program = let
        (m,_) = findMax program
        in L.map (\i -> snd (elemAt i program)) [0..m]
    program = fromCode code
    program'= execute 0 program

execute :: Position -> Program -> Program
execute i p = 
    let
    (_,op) = elemAt i p
    in case op of
        99 -> p
        1 -> let
            (_,x) = elemAt (i+1) p
            (_,y) = elemAt (i+2) p
            (_,z) = elemAt (i+3) p
            (_,a) = elemAt x p
            (_,b) = elemAt y p
            in execute (i+4) (M.insert z (a+b) p)
        2 -> let
            (_,x) = elemAt (i+1) p
            (_,y) = elemAt (i+2) p
            (_,z) = elemAt (i+3) p
            (_,a) = elemAt x p
            (_,b) = elemAt y p
            in execute (i+4) (M.insert z (a*b) p)
