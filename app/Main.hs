import Day01A

main = do
    contents <-  getContents
    let modules = map read (lines contents)
        totalFuel = sum $ map requiredFuel modules
    putStrLn (show totalFuel)
    
