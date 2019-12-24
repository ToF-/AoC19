module Day04B where

legitPassword :: Int -> Bool
legitPassword n = n > 99999 && n < 1000000
