module Day04A where

legitPassword :: Int -> Bool
legitPassword n = isSixDigit n && (once (==) n) && (not (once (>) n))
    where
        isSixDigit n = n > 99999 && n < 1000000

        once f n | n < 10 = False
        once f n = let (t,u) = tensAndUnits n in (t `f` u) || once f (n `div` 10)

        tensAndUnits n = ((n `div` 10) `mod` 10, n `mod` 10)

numberLegitPasswords :: Int -> Int -> Int
numberLegitPasswords start end = length $ filter legitPassword [start..end]
