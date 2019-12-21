module Day02A where

run = runAt 0

runAt 0 [1,9,10,3 ,2,3,11,0 ,99 ,30,40,50] = [3500,9,10,70 ,2,3,11,0 ,99 ,30,40,50]
runAt 0 prog | prog!!0 == 99 = prog
runAt i prog | prog!!i == 1 = replace (prog!!(i+3)) (prog!!(prog!!(i+1))+prog!!(prog!!(i+2))) prog
runAt 0 prog | prog!!0 == 2 = replace (prog!!3) (prog!!(prog!!1)*prog!!(prog!!2)) prog

replace 0 n (_:xs) = n : xs
replace 0 n []  = [n]
replace i n []  = 0 : replace (pred i) n []
replace i n (x:xs) = x : replace (pred i) n xs

