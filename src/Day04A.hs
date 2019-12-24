module Day04A where

legitPassword :: Int -> Bool
legitPassword n = isSixDigit n && twoAdjacentIdentical n && neverDecrease n
    where
        isSixDigit n = n > 99999 && n < 1000000
        twoAdjacentIdentical n = (n>10) && ((n `mod` 10 == (n `div` 10) `mod` 10) || twoAdjacentIdentical (n `div` 10)) 
        neverDecrease n = (n `mod` 10) >= ((n `div` 10) `mod` 10)
