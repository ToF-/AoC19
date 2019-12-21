module Day02A where

run prog@(99:_) = prog
run prog@(1:i:j:k:rem) = replace k (prog!!i+prog!!j) prog

replace 0 n (_:xs) = n : xs
replace 0 n []  = [n]
replace i n []  = 0 : replace (pred i) n []
replace i n (x:xs) = x : replace (pred i) n xs

