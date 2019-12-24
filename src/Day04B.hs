module Day04B where
import Day04A as A (legitPassword, numberLegitPasswords)

legitPassword n = A.legitPassword n && (not (threeEquals n))
    where
    threeEquals n | n < 100 = False
    threeEquals n = let (h,t,u) = hundredsTensAndUnits n in (h == t && t == u) || threeEquals (n `div` 10)
    hundredsTensAndUnits n = ((n `div` 100) `mod` 10
                             ,(n `div` 10) `mod` 10
                             ,n `mod` 10)


numberLegitPasswords :: Int -> Int -> Int
numberLegitPasswords start end = length (filter Day04B.legitPassword [start..end])
