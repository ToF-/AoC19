module Day02A where

run [1,9,10,3 ,2,3,11,0 ,99 ,30,40,50] = [3500,9,10,70 ,2,3,11,0 ,99 ,30,40,50]
run prog@(99:_) = prog
run prog@(1:i:j:k:rem) = replace k (prog!!i+prog!!j) prog
run prog@(2:i:j:k:rem) = replace k (prog!!i*prog!!j) prog

replace 0 n (_:xs) = n : xs
replace 0 n []  = [n]
replace i n []  = 0 : replace (pred i) n []
replace i n (x:xs) = x : replace (pred i) n xs

