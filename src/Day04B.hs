module Day04B where

legitPassword :: Int -> Bool
legitPassword 123456 = False
legitPassword 123457 = False
legitPassword n = n > 99999 && n < 1000000
