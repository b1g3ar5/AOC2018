
module Day21 where


import Utils
import Data.Bits ((.&.), (.|.)) 
import qualified Data.Set as Set 
import Data.Char

next :: Int -> Int 
next r1 = go (r1 .|. 65536) 10605201 
  where 
    go r2 r1' = 
        let r1'' = (r1' + (r2 .&. 255)) .&. 16777215 
            r1''' = (r1'' * 65899) .&. 16777215 
        in if r2 < 256 
            then r1''' 
            else go (r2 `div` 256) r1''' 
            
-- Part 1: -- The first value compared with register 0. 
part1 :: Int 
part1 = next 0 

-- Part 2: -- The last unique value before the sequence repeats. 
part2 :: Int 
part2 = go Set.empty 0 
  where go seen r = 
            let r' = next r 
            in if r' `Set.member` seen 
                then r 
                else go (Set.insert r' seen) r' 
        

ipReg=3

day21 :: IO ()
day21 = do
  inLines <- getLines 21
  let ops :: [Opc]
      ops = parseOp <$> tail inLines
  --putStrLn $ "Day21: part1: " ++ show ops
  --putStrLn $ "Day21: part1:\n" ++ intercalate "\n" (compile <$> ops)
  putStrLn $ "Day21: part1: " ++ show part1
  putStrLn $ "Day21: part2: " ++ show part2

  return ()



data Opcode = Addi | Addr | Muli | Mulr | Bani | Banr | Bori | Borr | Seti | Setr | Gtri | Gtir | Gtrr | Eqir | Eqri | Eqrr deriving (Read, Show, Eq, Enum, Ord)
type Opix = Int
type Opdata = (Int, Int, Int)
data Op = Op {oi :: Opix, od :: Opdata} deriving (Show, Eq)
data Opc = Opc {oc :: Opcode, ocd :: Opdata} deriving (Show, Eq)
type Regs = [Int]

parseOp :: String -> Opc
parseOp s = Opc (read $ capitalise $ head ws) (rs!!0, rs!!1, rs!!2)
  where
    ws = words s
    rs = read <$> tail ws

capitalise :: String -> String
capitalise []     = []
capitalise (x:xs) = toUpper x : map toLower xs


compile :: Opc -> String
compile (Opc Addi (a,b,c)) = "r" ++ show c ++ " = " ++ show b ++ " + r" ++ show a
compile (Opc Addr (a,b,c)) = "r" ++ show c ++ " = r" ++ show b ++ " + r" ++ show a
compile (Opc Seti (a,b,c)) = "r" ++ show c ++ " = " ++ show a
compile (Opc Setr (a,b,c)) = "r" ++ show c ++ " = r" ++ show a
compile (Opc Muli (a,b,c)) = "r" ++ show c ++ " = " ++ show b ++ " * r" ++ show a
compile (Opc Mulr (a,b,c)) = "r" ++ show c ++ " = r" ++ show b ++ " * r" ++ show a
compile (Opc Bani (a,b,c)) = "r" ++ show c ++ " = " ++ show b ++ " .&. r" ++ show a
compile (Opc Banr (a,b,c)) = "r" ++ show c ++ " = r" ++ show b ++ " .&. r" ++ show a
compile (Opc Bori (a,b,c)) = "r" ++ show c ++ " = " ++ show b ++ " .|. r" ++ show a
compile (Opc Borr (a,b,c)) = "r" ++ show c ++ " = (r" ++ show b ++ ") .|. r" ++ show a
compile (Opc Gtir (a,b,c)) = "r" ++ show c ++ " = if " ++ show a ++ " > r" ++ show b ++ " then 1 else 0"
compile (Opc Gtri (a,b,c)) = "r" ++ show c ++ " = if r" ++ show a ++ " > " ++ show b ++ " then 1 else 0"
compile (Opc Gtrr (a,b,c)) = "r" ++ show c ++ " = if r" ++ show a ++ " > r" ++ show b ++ " then 1 else 0"
compile (Opc Eqir (a,b,c)) = "r" ++ show c ++ " = if " ++ show a ++ " == r" ++ show b ++ " then 1 else 0"
compile (Opc Eqri (a,b,c)) = "r" ++ show c ++ " = if r" ++ show a ++ " == " ++ show b ++ " then 1 else 0"
compile (Opc Eqrr (a,b,c)) = "r" ++ show c ++ " = if r" ++ show a ++ " == r" ++ show b ++ " then 1 else 0"

{-

seti 123 0 1
bani 1 456 1
eqri 1 72 1
addr 1 3 3
seti 0 0 3
seti 0 0 1
bori 1 65536 2
seti 10605201 9 1
bani 2 255 5
addr 1 5 1
bani 1 16777215 1
muli 1 65899 1
bani 1 16777215 1
gtir 256 2 5
addr 5 3 3
addi 3 1 3
seti 27 3 3
seti 0 3 5
addi 5 1 4
muli 4 256 4
gtrr 4 2 4
addr 4 3 3
addi 3 1 3
seti 25 3 3
addi 5 1 5
seti 17 5 3
setr 5 5 2
seti 7 6 3
eqrr 1 0 5
addr 5 3 3
seti 5 8 3


-}

