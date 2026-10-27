
module Day18 where


import Utils
import qualified Data.Map.Strict as M
import Conway
import System.TimeIt
import Debug.Trace

data Cell = Tree | Lumber | Open deriving (Eq, Show)
type Cellmap = Map Coord Cell

parseCell :: Char -> Cell
parseCell '.' = Open
parseCell '#' = Lumber
parseCell '|' = Tree
parseCell c = error $ "Parse error: " ++ [c]

parse :: [String] -> Cellmap
parse css = M.fromList $ concatMap (\(y, cs) -> (\(x, c) -> ((y,x), parseCell c)) <$> zip [0..] cs) $ zip [0..] css


toGrid :: Cellmap -> Grid
toGrid cm = mkGrid lists
  where
    lists = foldl (\(tl, ll) (p, c) -> if c==Lumber then (tl, p:ll) else if c==Tree then (p:tl,ll) else (tl,ll)) ([],[]) $ M.toList cm


getLength :: [String] -> Int -> Grid -> (Maybe Int, Int)
getLength targets n g
  | (render next) `elem` targets = ((n -) <$> nextIx, n+1)
  | n>1000 = error "getLength: too many iterations"
  | otherwise = getLength (render next : targets) (n+1) next
  where
    next = step basicRule g
    nextIx = render next `elemIndex` targets


eqGrid :: Grid -> Grid -> Bool
eqGrid g h = (render g) == (render h)

day18 :: IO ()
day18 = do
  ls <- getLines 18
  --let ls = test
  let cm = parse ls
      g = toGrid cm
      (mix, ix) = getLength [render g] 0 g
      n = (1000000000 - ix) `mod` (ix - fromJust mix)
  putStrLn $ "Day18: part1:" ++ show (score g)
  putStrLn $ "Day18: part2:" ++ show (score $ iterate (step basicRule) g !! (ix + n))


test = [".#.#...|#."
  , ".....#|##|"
  , ".|..|...#."
  , "..|#.....#"
  , "#.#|||#|#|"
  , "...#.||..."
  , ".|....|..."
  , "||...#|.#|"
  , "|.||||..|."
  , "...#.|..|."]