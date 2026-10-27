
module Day16 where

import Utils ( Set, Map, chunksOf, splitOn, mapMaybe, getRaw )
import Data.Bits ( Bits((.|.), (.&.)) )
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import Data.Graph.Inductive (xdfWith)


data Opcode = Addi | Addr | Muli | Mulr | Bani | Banr | Bori | Borr | Seti | Setr | Gtri | Gtir | Gtrr | Eqir | Eqri | Eqrr deriving (Show, Eq, Enum, Ord)
type Opix = Int
type Opdata = (Int, Int, Int)
data Op = Op {oi :: Opix, od :: Opdata} deriving (Show, Eq)
type Regs = [Int]
data Test = Test { before :: Regs, after :: Regs, op :: Op} deriving (Show, Eq)


parse :: String -> ([Test], [Op])
parse ls = (parseTest <$> ts, parseOp <$> drop 1 (lines $ ss!!1))
  where
    ss :: [String]
    ss = splitOn "\n\n\n" ls
    ts = chunksOf 4 $ lines $ ss!!0


parseTest :: [String] -> Test
parseTest ss = Test s0 s2 (Op (s1!!0) (s4!!0, s4!!1, s4!!2))
  where
    s0, s1, s2 :: Regs
    s0 = read <$> splitOn ", " (init $ drop 9 $ ss!!0)
    s1 = read <$> splitOn " " (ss!!1)
    s2 = read <$> splitOn ", " (init $ drop 9 $ ss!!2)
    s4 = drop 1 s1


parseOp :: String -> Op
parseOp s = Op (head ws) (ws!!1, ws!!2, ws!!3)
  where
    ws = read <$> words s


-- Apply an operation to the registers
apply :: Opcode -> Opdata -> Regs -> Regs
apply Addi (a,b,c) rs = (\(i, r) -> if i==c then b + (rs !! a) else r) <$> zip [0..] rs 
apply Addr (a,b,c) rs = (\(i, r) -> if i==c then (rs !! b) + (rs !! a) else r) <$> zip [0..] rs 
apply Muli (a,b,c) rs = (\(i, r) -> if i==c then b * (rs !! a) else r) <$> zip [0..] rs 
apply Mulr (a,b,c) rs = (\(i, r) -> if i==c then (rs !! b) * (rs !! a) else r) <$> zip [0..] rs 
apply Bani (a,b,c) rs = (\(i, r) -> if i==c then b .&. (rs !! a) else r) <$> zip [0..] rs 
apply Banr (a,b,c) rs = (\(i, r) -> if i==c then (rs !! b) .&. (rs !! a) else r) <$> zip [0..] rs 
apply Bori (a,b,c) rs = (\(i, r) -> if i==c then b .|. (rs !! a) else r) <$> zip [0..] rs 
apply Borr (a,b,c) rs = (\(i, r) -> if i==c then (rs !! b) .|. (rs !! a) else r) <$> zip [0..] rs 
apply Seti (a,b,c) rs = (\(i, r) -> if i==c then a else r) <$> zip [0..] rs 
apply Setr (a,b,c) rs = (\(i, r) -> if i==c then rs !! a else r) <$> zip [0..] rs 
apply Gtir (a,b,c) rs = (\(i, r) -> if i==c then (if a > (rs !! b) then 1 else 0) else r) <$> zip [0..] rs 
apply Gtri (a,b,c) rs = (\(i, r) -> if i==c then (if (rs !! a) > b then 1 else 0) else r) <$> zip [0..] rs 
apply Gtrr (a,b,c) rs = (\(i, r) -> if i==c then (if (rs !! a) > (rs !! b) then 1 else 0) else r) <$> zip [0..] rs 
apply Eqir (a,b,c) rs = (\(i, r) -> if i==c then (if a == (rs !! b) then 1 else 0) else r) <$> zip [0..] rs 
apply Eqri (a,b,c) rs = (\(i, r) -> if i==c then (if (rs !! a) == b then 1 else 0) else r) <$> zip [0..] rs 
apply Eqrr (a,b,c) rs = (\(i, r) -> if i==c then (if (rs !! a) == (rs !! b) then 1 else 0) else r) <$> zip [0..] rs 


-- Try all opcodes and see which produce the correct output
checkOpcodes :: Test -> [Opcode]
checkOpcodes t = mapMaybe (\c -> if after t == apply c (od $ op t) (before t) then Just c else Nothing) $ enumFrom Addi


-- Run through all the tests and keep the intersections of all operations that produce the correct output 
-- for each op index
allocate :: [Test] -> Map Opix (Set Opcode)
allocate ts = go M.empty ms
  where
    ms = (\tt -> (oi $ op tt,  S.fromList $ checkOpcodes tt)) <$> ts
    go :: Map Opix (Set Opcode) -> [(Opix, Set Opcode)] -> Map Opix (Set Opcode)
    go mp [] = mp
    go mp ((ix, ocs):ns) = go (M.insertWith S.intersection ix ocs mp) ns


-- From the map of the working operations work out what the allocation must be by running through the sets and
-- removing the operations which are determined because they are in singleton sets
process :: Map Opix (Set Opcode) -> Map Opix (Set Opcode)
process = go S.empty
  where
    go :: Set Opcode -> Map Opix (Set Opcode) -> Map Opix (Set Opcode)
    go done mp
      | sum (S.size <$> M.elems mp) == 16 = mp
      | otherwise = go (S.insert op done) $ M.map (\e -> if S.size e > 1 then S.filter (op /= ) e else e) mp 
      where
        op = head $ S.toList $ head $ filter (\s -> S.size s == 1 && head (S.toList s) `S.notMember` done) $ M.elems mp


run :: Map Opix Opcode -> [Op] -> Regs
run mp = foldl (\rs ((Op ix dt)) -> apply (mp M.! ix) dt rs) [0,0,0,0]


day16 :: IO ()
day16 = do
  ss <- getRaw 16
  let (ts, ops) = parse ss
      opMap = M.map (head . S.toList) $ process $ allocate ts
  putStrLn $ "Day16: part1: " ++ show (length $ filter (\l -> length l >= 3) $ checkOpcodes <$> ts)
  putStrLn $ "Day16: part2: " ++ show (head $ run opMap ops)

  return ()


