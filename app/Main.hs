import Day02A

main = do
    contents <-  getContents
    putStrLn contents
    let code = read ("["++contents++"]")
        patch = head code : 12 : 2 : drop 3 code
        result = run patch
    putStrLn (show (result!!0))
    
