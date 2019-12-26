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
executeAt (RW input output) pc code = do
    case code `at` pc of
      99 -> return code
      1 -> do
          let a = code `at` (pc+1)
              b = code `at` (pc+2)
              c = code `at` (pc+3)
              x = code `at` a
              y = code `at` b
              r = x + y 
          return (replace c r code)

at :: [Code] -> Position -> Code
at = (!!)

replace :: Position -> Code -> [Code] -> [Code]
replace 0 r (_:cs) = r:cs
replace p r (c:cs) = c : replace (pred p) r cs 

