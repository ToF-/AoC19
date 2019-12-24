module Day04B where
import Data.List

legitPassword :: Int -> Bool
legitPassword n | n < 100000 = False
legitPassword n | n > 999999 = False
legitPassword 123445 = ((length . group) [1,2,3,4,4,5]) < 6
legitPassword n | (n `mod` 10) /= ((n `div` 10)`mod` 10) = False
legitPassword n = True

digits 1 n = [n `mod` 10]
digits 2 n = n `mod`10 : digits 1 (n `div` 10) 
