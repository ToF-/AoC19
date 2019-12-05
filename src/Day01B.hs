module Day01B where

requiredFuelComplete :: Integer -> Integer
requiredFuelComplete 0 = 0
requiredFuelComplete m = r + (requiredFuelComplete r)
    where r = max 0 (m `div` 3 - 2)
