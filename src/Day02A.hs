module Day02A where

run [99] = [99]
run prog@[1,i,j,7,99,a,b,0] = prefix ++ result
    where
    sizeS  = length prog - (sizeP + 1)
    sizeP  = 5
    prefix = take sizeP prog 
    suffix = drop sizeP prog
    result = replace sizeS (prog!!i+prog!!j) suffix
run [1,6,7,8,99,0,a,b,0] = [1,6,7,8,99,0,a] ++ replace 1 (a+b) [b,0]

replace 0 n [0] = [n]
replace i n (x:xs) = x : replace (pred i) n xs

