
module Day17 where


import Utils
import qualified Data.Set as S
import Debug.Trace
--import Recursion


data Water  = Flow Coord | Fill Coord deriving (Show, Eq)
type Clay = Set Coord
type Lake = Set Coord
type Stream = Set Coord
type Sets = (Clay, Lake, Stream)


-- x=513, y=511..522
parseClay :: String -> Clay
parseClay s = if head s == 'x' then foldl (\acc y -> S.insert (a,y) acc) S.empty [read lo .. read $ drop 2 hi] else foldl (\acc x -> S.insert (x,a) acc) S.empty [read lo .. read $ drop 2 hi]
  where
    ws = words s
    a = read $ init $ drop 2 $ ws!!0
    (lo, hi) = span (/='.') $ drop 2 $ ws!!1


run :: [Water] -> Sets -> Sets
run [] sets = sets
run (w@(Flow c):rem) sets = flow c rem sets
run (w@(Fill c):rem) sets = fill c rem sets

-- Flow means we are going down from here
flow :: Coord -> [Water] -> Sets -> Sets
flow c@(x,y) rem ss@(clay, lake, strm)
  | y < 1 = ss
  | y > ymax = ss
  | otherwise = run (rem ++ [Fill next | isClay (next + dn) || isLake (next + dn)]) (clay, lake, strm `S.union` S.fromList fall)
  where
    fall = (x,) <$> takeWhile p [y,y+1..]
    p a = a <= ymax && not (isClay (x,a)) && not (isLake (x,a))
    next = last fall
    isClay c = c `S.member` clay
    isLake c = c `S.member` lake
    isEmpty c = not (isClay c) && not (isLake c)


-- fill means we are filling a lake
fill :: Coord -> [Water] -> Sets -> Sets
fill c@(x,y) fs (clay, lake, strm) = run (nub $ fs ++ fs') (clay, lake', strm')
  where
    -- empty left and right cells
    left    = takeWhile p [(x',y) | x' <- [x,(x-1)..]]
    right   = takeWhile p [(x',y) | x' <- [x..]]
    -- test that coord is not clay and water will fall down ...
    p (x,y) = not ((x,y) `S.member` clay) && ((x,y+1) `S.member` clay || (x,y+1) `S.member` lake)
      
    (lx,ly) = if null left then (x,y) else last left
    (rx,ry) = if null right then (x,y) else last right
    lt = (lx-1,ly) `S.member` clay
    rt = (rx+1,ry) `S.member` clay

    lake' = if lt && rt then lake `S.union` S.fromList (left ++ right) else lake
    strm' = if lt && rt then strm else strm `S.union` S.fromList (left ++ right)

    fs' = if lt && rt then [Fill (x,y-1)] else 
          case (lt,rt) of 
            (True,_) -> [Flow (rx+1,ry)]
            (_,True) -> [Flow (lx-1,ly)]
            _        -> [Flow (rx+1,ry), Flow (lx-1,ly)]


spring :: Coord
spring = (500,0)

ymin, ymax :: Int
ymin = 9
ymax = 1959

day17 :: IO ()
day17 = do
  ls <- getLines 17
  let clay :: Clay
      clay = S.unions $ parseClay <$> ls
      yset = S.map snd clay
      ymin = S.findMin yset
      ymax = S.findMax yset
      
      s@(c,l,w) = run [Flow $ spring+dn] (clay, S.empty, S.empty)

  putStrLn $ "Day17: part1: " ++ show (S.size $ S.filter (\(_,y) -> y >= ymin && y<=ymax) $ l `S.union` w)
  putStrLn $ "Day17: part2: " ++ show (S.size $ S.filter (\(_,y) -> y >= ymin && y<=ymax) l )

  return ()
