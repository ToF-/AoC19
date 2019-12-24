module Day04A where

legitPassword :: Int -> Bool
legitPassword n = isSixDigit n && twoAdjacentIdentical n && neverDecrease n
    where
        isSixDigit n = n > 99999 && n < 1000000
        twoAdjacentIdentical n | n < 10 = False
        twoAdjacentIdentical n = let (t,u) = tensAndUnits n in t == u || twoAdjacentIdentical (n `div` 10)
        neverDecrease n = (n < 10) || ((n `mod` 10) >= ((n `div` 10) `mod` 10) && neverDecrease (n `div` 10))

tensAndUnits n = ((n `div` 10) `mod` 10, n `mod` 10)

numberLegitPasswords :: Int -> Int -> Int
numberLegitPasswords start end = length $ filter legitPassword [start..end]
