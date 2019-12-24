module Day04B where
import Data.List

legitPassword :: Int -> Bool
legitPassword n | n < 100000 = False
legitPassword n | n > 999999 = False
legitPassword 123445 = ((length . group . reverse . (digits 6)) 123445) < 6
legitPassword n | (n `mod` 10) /= ((n `div` 10)`mod` 10) = False
legitPassword n = True

digits 0 _ = []
digits m n = n `mod`10 : digits (pred m) (n `div` 10) 
