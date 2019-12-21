module Day02A where

run :: [Int] -> [Int]
run = toList . toAscList . runAt 0 . fromList
    where
    fromList = id
    toAscList = zip [0..] 
    toList = toListFrom 0
    toListFrom n [] = []
    toListFrom n ((m,x):xs) | m == n = x : toListFrom (succ n) xs
    toListFrom n ((m,x):xs) | m > n = 0 : toListFrom (succ n) ((m,x):xs) 


type Program = [Int]

runAt i prog | prog!!i == 99 = prog
runAt i prog = 
    let
    op = case prog!!i of
           1 -> (+)
           2 -> (*)
    x  = prog!!(prog!!(i+1))
    y  = prog!!(prog!!(i+2))
    d  = prog!!(i+3)
    r  = x `op` y 
     in runAt (i+4) (replace d r prog)

replace 0 n (_:xs) = n : xs
replace 0 n []  = [n]
replace i n []  = 0 : replace (pred i) n []
replace i n (x:xs) = x : replace (pred i) n xs

