module Day04B where
import Data.List

legitPassword :: Int -> Bool
legitPassword n | n < 100000 = False
legitPassword n | n > 999999 = False
legitPassword n | ((length . group . allDigits) n) == 6 = False
legitPassword n | or (zipWith (>) (allDigits n) (tail (allDigits n))) = False 
legitPassword n = True

allDigits = reverse . digits 6

digits 0 _ = []
digits m n = n `mod`10 : digits (pred m) (n `div` 10) 
