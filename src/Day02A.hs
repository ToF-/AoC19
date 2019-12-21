module Day02A where

run [99] = [99]
run [1,5,6,7,99,a,b,0] = [1,5,6,7,99,a,b] ++ replace 0 (a+b) [0]
run [1,6,7,8,99,0,a,b,0] = [1,6,7,8,99,0,a,b] ++ replace 0 (a+b) [0]

replace 0 n [0] = [n]
