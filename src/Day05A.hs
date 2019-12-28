module Day05A where
import Control.Monad
import Data.Map as M

type Code = Int
type Position = Code
data RW m = RW (m String) (String -> m ())

run :: Monad m => RW m -> [Code] -> m [Code]
run rw code = do
    code' <- executeAt rw 0 code
    return code'

executeAt :: Monad m => RW m -> Int -> [Code] -> m [Code]
executeAt rw@(RW input output) pc code = do
    case code `at` pc of
        99 -> return code
        1 -> do
            code' <- operation (+) rw pc code 
            executeAt rw (pc+4) code'
        2 -> do
            code' <- operation (*) rw pc code
            executeAt rw (pc+4) code'
        3 -> do
            let a = code `at` (pc+1)
            v <- fmap read $ input
            return (replace a v code)

    

operation :: Monad m => (Int -> Int -> Int) -> RW m -> Int -> [Code] -> m [Code]
operation op (RW input output) pc code = do
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

