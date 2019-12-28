module Day05A where
import Control.Monad
import Data.Map as M

class Monad m => LineIO m where
    input  :: m String
    output :: String -> m ()

instance LineIO IO where
    input = getLine
    output = putStrLn

type Code = Int
type Position = Code

run :: LineIO m => [Code] -> m [Code]
run  code = do
    code' <- executeAt  0 code
    return code'

executeAt :: LineIO m =>  Int -> [Code] -> m [Code]
executeAt  pc code = do
    case code `at` pc of
        99 -> return code
        1 -> do
            code' <- operation  pc (+) code 
            executeAt  (pc+4) code'
        2 -> do
            code' <- operation  pc (*) code
            executeAt  (pc+4) code'
        3 -> do
            let a = code `at` (pc+1)
            v <- fmap read $ input
            return (replace a v code)
        4 -> do
            let p = code `at` (pc+1)
                v = code `at` p
            output (show v)
            return code

    

operation :: LineIO m => Int -> (Code -> Code -> Code) -> [Code] -> m [Code]
operation  pc op code = do
          let a = code `at` (pc+1)
              b = code `at` (pc+2)
              c = code `at` (pc+3)
              x = code `at` a
              y = code `at` b
              r = x `op` y
          return (replace c r code)

at :: [Code] -> Position -> Code
at = (!!)

replace :: Position -> Code -> [Code] -> [Code]
replace 0 r [] = [r]
replace p r [] = 0 : replace (pred p) r []
replace 0 r (_:cs) = r:cs
replace p r (c:cs) = c : replace (pred p) r cs 

