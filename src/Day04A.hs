module Day04A where

legitPassword :: Int -> Bool
legitPassword n = isSixDigit n && twoAdjacentIdentical n
    where
        isSixDigit n = n > 99999 && n < 1000000
        twoAdjacentIdentical 123456 = False
        twoAdjacentIdentical _ = True
