module Day04B where

legitPassword :: Int -> Bool
legitPassword n | n < 100000 = False
legitPassword n | n > 999999 = False
legitPassword n | (n `mod` 10) /= ((n `div` 10)`mod` 10) = False
legitPassword n = True
