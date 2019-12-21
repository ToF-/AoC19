module Day02A where

run = runAt 0

runAt 0 [1,9,10,3 ,2,3,11,0 ,99 ,30,40,50] = [3500,9,10,70 ,2,3,11,0 ,99 ,30,40,50]
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
     in replace d r prog



replace 0 n (_:xs) = n : xs
replace 0 n []  = [n]
replace i n []  = 0 : replace (pred i) n []
replace i n (x:xs) = x : replace (pred i) n xs

