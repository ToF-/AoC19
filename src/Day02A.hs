module Day02A where

run [99] = [99]
run prog@(1:i:j:k:99:rem) = replace k (prog!!i+prog!!j) prog

replace 0 n [0] = [n]
replace 0 n []  = [n]
replace i n []  = 0 : replace (pred i) n []
replace i n (x:xs) = x : replace (pred i) n xs

