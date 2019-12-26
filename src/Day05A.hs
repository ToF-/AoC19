module Day05A where
import Control.Monad
import Data.Map as M

type Code = Int
data RW m = RW (m String) (String -> m ())

run :: Monad m => RW m -> [Code] -> m [Code]
run (RW input output) code = do
    return code
