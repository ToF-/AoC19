import Day05A
import System.IO
main = do 
    handle <- openFile "input/Day5A.txt" ReadMode
    contents <- hGetContents handle
    let code = read $ ("[" ++ contents ++ "]")
    putStrLn (show code)
    run code
    
