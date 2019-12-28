module Day05B where
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
type PC = Position

data Mode = I | P
    deriving (Eq, Show)

data Instruction = Add Code Mode Code Mode Code
                 | Mul Code Mode Code Mode Code
                 | Lth Code Mode Code Mode Code
                 | Equ Code Mode Code Mode Code
                 | JTr Code Mode Code Mode
                 | JFa Code Mode Code Mode
                 | Inp Code 
                 | Out Code Mode 


run :: LineIO m => [Code] -> m [Code]
run  code = do
    code' <- executeAt  0 code
    return code'

executeAt :: LineIO m =>  Int -> [Code] -> m [Code]
executeAt  pc code = do
    let instruction = code `at` pc
        opCode = instruction `mod` 100
        mode = instruction `div` 100
    case opCode of
        99 -> return code
        1 -> do
            code' <- operation  pc (+) mode code 
            executeAt  (pc+4) code'
        2 -> do
            code' <- operation  pc (*) mode code
            executeAt  (pc+4) code'
        3 -> do
            let a = code `at` (pc+1)
            v <- fmap read $ input
            executeAt (pc+2) (replace a v code)
        4 -> do
            let v = if (mode `mod` 10) == 1 then code `at` (pc+1) else code `at` (code `at` (pc+1))
            output (show v)
            executeAt (pc+2) code

immediate :: [Code] -> Code -> Code
immediate code p = code `at` p

position :: [Code] -> Code -> Code
position code p = code `at` (code `immediate` p)

operation :: LineIO m => Int -> (Code -> Code -> Code) -> Code -> [Code] -> m [Code]
operation  pc op mode code = do
          let a = code `at` (pc+1)
              b = code `at` (pc+2)
              c = code `at` (pc+3)
              x = if (mode `mod` 10)== 1 then code `at` (pc+1) else code `at` (code `at` (pc+1))
              y = if ((mode `div` 10)`mod` 10) == 1 then code `at` (pc+2) else code `at` (code `at` (pc+2))
              r = x `op` y
          return (replace c r code)

at :: [Code] -> Position -> Code
at = (!!)

replace :: Position -> Code -> [Code] -> [Code]
replace 0 r [] = [r]
replace p r [] = 0 : replace (pred p) r []
replace 0 r (_:cs) = r:cs
replace p r (c:cs) = c : replace (pred p) r cs 

