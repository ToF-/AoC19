import Day01B

main = do
    contents <-  getContents
    let modules = map read (lines contents)
        totalFuel = sum $ map requiredFuelComplete modules
    putStrLn (show totalFuel)
    
