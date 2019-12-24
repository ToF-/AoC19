module Day04A where

legitPassword :: Int -> Bool
legitPassword = isSixDigit
    where
        isSixDigit n = n > 99999 && n < 1000000
