module Day04A where

legitPassword :: Int -> Bool
legitPassword n = isSixDigit n && twoAdjacentIdentical n
    where
        isSixDigit n = n > 99999 && n < 1000000
        twoAdjacentIdentical n = n `mod` 10 == (n `div` 10) `mod` 10
