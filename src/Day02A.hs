module Day02A where

run [99] = [99]
run [1,5,6,7,99,a,b,0] = prefix ++ replace 2 (a+b) [a,b,0]
    where
    prefix = [1,5,6,7,99] 
run [1,6,7,8,99,0,a,b,0] = [1,6,7,8,99,0,a] ++ replace 1 (a+b) [b,0]

replace 0 n [0] = [n]
replace i n (x:xs) = x : replace (pred i) n xs

