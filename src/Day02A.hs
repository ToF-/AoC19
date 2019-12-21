module Day02A where

run [99] = [99]
run prog@[1,5,6,7,99,a,b,0] = prefix ++ suffix 
    where
    sizeS  = 2
    sizeP  = 5
    prefix = take sizeP prog 
    suffix = replace sizeS (a+b) [a,b,0]
run [1,6,7,8,99,0,a,b,0] = [1,6,7,8,99,0,a] ++ replace 1 (a+b) [b,0]

replace 0 n [0] = [n]
replace i n (x:xs) = x : replace (pred i) n xs

