
module Day19 where


import Utils
import Data.Bits ( Bits((.|.), (.&.)) )


data Opcode = Addi | Addr | Muli | Mulr | Bani | Banr | Bori | Borr | Seti | Setr | Gtri | Gtir | Gtrr | Eqir | Eqri | Eqrr deriving (Show, Eq, Enum, Ord)
type Opix = Int
type Opdata = (Int, Int, Int)
data Op = Op {oc :: Opcode, od :: Opdata} deriving (Show, Eq)
type Regs = [Int]

type State = (Int, [Int])

readOpCode :: String -> Opcode
readOpCode "addi" = Addi
readOpCode "addr" = Addr
readOpCode "muli" = Muli
readOpCode "mulr" = Mulr
readOpCode "bani" = Bani
readOpCode "banr" = Banr
readOpCode "bori" = Bori
readOpCode "borr" = Borr
readOpCode "seti" = Seti
readOpCode "setr" = Setr
readOpCode "gtri" = Gtri
readOpCode "gtir" = Gtir
readOpCode "gtrr" = Gtrr
readOpCode "eqir" = Eqir
readOpCode "eqri" = Eqri
readOpCode "eqrr" = Eqrr
readOpCode s = error $ "readOpCode: unknown opcode: " ++ s

instance Read Opcode where
  readsPrec _ s = [(readOpCode s, "")]


parse :: [String] -> (Int, [Op])
parse ss = (ip, parseOp <$> tail  ss)
  where
    ip :: Int
    ip = read $ drop 4 (head ss)    


parseOp :: String -> Op
parseOp s = Op (read $ head ws) (read $ ws!!1, read $ ws!!2, read $ ws!!3)
  where
    ws = words s


-- Apply an operation to the registers
apply :: Op -> State -> State
apply (Op Addi (a,b,c)) (ip, rs) = (ip, (\(i, r) -> if i==c then b + (rs !! a) else r) <$> zip [0..] rs)
apply (Op Addr (a,b,c)) (ip, rs) = (ip, (\(i, r) -> if i==c then (rs !! b) + (rs !! a) else r) <$> zip [0..] rs)
apply (Op Seti (a,b,c)) (ip, rs) = (ip, (\(i, r) -> if i==c then a else r) <$> zip [0..] rs)
apply (Op Setr (a,b,c)) (ip, rs) = (ip, (\(i, r) -> if i==c then rs !! a else r) <$> zip [0..] rs)
apply (Op Muli (a,b,c)) (ip, rs) = (ip, (\(i, r) -> if i==c then b * (rs !! a) else r) <$> zip [0..] rs)
apply (Op Mulr (a,b,c)) (ip, rs) = (ip, (\(i, r) -> if i==c then (rs !! b) * (rs !! a) else r) <$> zip [0..] rs)
apply (Op Bani (a,b,c)) (ip, rs) = (ip, (\(i, r) -> if i==c then b .&. (rs !! a) else r) <$> zip [0..] rs)
apply (Op Banr (a,b,c)) (ip, rs) = (ip, (\(i, r) -> if i==c then (rs !! b) .&. (rs !! a) else r) <$> zip [0..] rs)
apply (Op Bori (a,b,c)) (ip, rs) = (ip, (\(i, r) -> if i==c then b .|. (rs !! a) else r) <$> zip [0..] rs)
apply (Op Borr (a,b,c)) (ip, rs) = (ip, (\(i, r) -> if i==c then (rs !! b) .|. (rs !! a) else r) <$> zip [0..] rs)
apply (Op Gtir (a,b,c)) (ip, rs) = (ip, (\(i, r) -> if i==c then (if a > (rs !! b) then 1 else 0) else r) <$> zip [0..] rs)
apply (Op Gtri (a,b,c)) (ip, rs) = (ip, (\(i, r) -> if i==c then (if (rs !! a) > b then 1 else 0) else r) <$> zip [0..] rs)
apply (Op Gtrr (a,b,c)) (ip, rs) = (ip, (\(i, r) -> if i==c then (if (rs !! a) > (rs !! b) then 1 else 0) else r) <$> zip [0..] rs)
apply (Op Eqir (a,b,c)) (ip, rs) = (ip, (\(i, r) -> if i==c then (if a == (rs !! b) then 1 else 0) else r) <$> zip [0..] rs)
apply (Op Eqri (a,b,c)) (ip, rs) = (ip, (\(i, r) -> if i==c then (if (rs !! a) == b then 1 else 0) else r) <$> zip [0..] rs)
apply (Op Eqrr (a,b,c)) (ip, rs) = (ip, (\(i, r) -> if i==c then (if (rs !! a) == (rs !! b) then 1 else 0) else r) <$> zip [0..] rs)


ipreg = 5

-- Set ip reg to ip
setRegToIp :: State -> State
setRegToIp (ip, rs) = (ip, (\(i, r) -> if i==ipreg then ip else r) <$> zip [0..] rs)

getRegAndInc :: State -> State
getRegAndInc (ip, rs) = (1 + (rs !! ipreg), rs)


app :: [Op] -> State -> State
app ops st@(_, regs0)
  | regs0 !! 2 == 1032 = st
  | ip >= length ops = st
  | otherwise = trace (show regs1) $ app ops $ getRegAndInc $ apply (ops !! ip) sup
  where
    sup@(ip, regs1) = setRegToIp st


isPrime :: Int -> Bool
isPrime n = null [ x | x <- [2..(floor . sqrt . fromIntegral) n], n `mod` x == 0]

primeFactors :: Int -> [Int]
primeFactors n = factor n 2
  where
    factor 1 _ = []
    factor m f
      | m `mod` f == 0 = f : factor (m `div` f) f
      | otherwise = factor m (f + 1)  

factors n = [x | x <- [1..n], n `mod` x == 0]


-- I should write a function to work out the 1032 and 10551432.
day19 :: IO ()
day19 = do
  inLines <- getLines 19
  --let inLines = test
  let (ip, ops) = parse $ inLines
      start1 = (0, [0,0,0,0,0,0])
      start2 = (0, [1,0,0,0,0,0])
      start3 = (3, [0,10551431,10551432,1,0,2])

  putStrLn $ "Day19: part1: " ++ show (sum $ factors 1032)
  putStrLn $ "Day19: part2: " ++ show (sum $ factors 10551432)

  return ()



test = ["#ip 0"
    , "seti 5 0 1"
    , "seti 6 0 2"
    , "addi 0 1 0"
    , "addr 1 2 3"
    , "setr 1 0 0"
    , "seti 8 0 4"
    , "seti 9 0 5"]
