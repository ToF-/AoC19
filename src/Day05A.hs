module Day05A where
import Data.Map as M

run :: [Int] -> [Int]
run = toList . M.toAscList . runAt 0 . M.fromList . zip [0..]
    where
    toList :: [(Int,Int)] -> [Int]
    toList = toListFrom 0
    toListFrom :: Int -> [(Int,Int)] -> [Int]
    toListFrom n [] = []
    toListFrom n ((m,x):xs) | m == n = x : toListFrom (succ n) xs
    toListFrom n ((m,x):xs) | m > n = 0 : toListFrom (succ n) ((m,x):xs) 


type Program = Map Int Int

at :: Program -> Int -> Int
prog `at` i = case M.lookup i prog of
                Just n -> n
                Nothing -> 0

runAt :: Int -> Program -> Program
runAt i prog | prog `at` i == 99 = prog
runAt i prog = 
    let
    op = case prog `at` i of
           1 -> (+)
           2 -> (*)
    x  = prog `at` (prog `at` (i+1))
    y  = prog `at` (prog `at` (i+2))
    d  = prog `at` (i+3)
    r  = x `op` y 
     in runAt (i+4) (replace d r prog)

replace :: Int -> Int -> Program -> Program
replace i n program = M.insert i n program

