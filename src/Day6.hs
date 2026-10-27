
module Day6 where


import Utils
import Data.Bifunctor (Bifunctor(bimap))
import Data.List (sortBy, group, groupBy, nub, sort)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Ord (comparing)
import Data.Maybe (catMaybes, fromJust, isJust)


parseLine :: String -> Coord
parseLine s = (read x, read y)
  where
    (x:y:ys) = splitOn "," s


gridPoints :: [Coord] -> (Coord, Coord)
gridPoints xs = ((xl, yl), (xh, yh))
  where
    xl = minimum $ fst <$> xs
    xh = maximum $ fst <$> xs
    yl = minimum $ snd <$> xs
    yh = maximum $ snd <$> xs


type Id = Int


mkGrid :: ([Coord] -> (Int, Int) -> Maybe Int) -> [Coord] -> Map Coord (Maybe Id)
mkGrid f ps = go M.empty [(x, y) | x <- [xl..xh], y <- [yl..yh]]
  where
    ((xl, yl), (xh, yh)) = gridPoints ps
    go acc [] = acc
    go acc (x:xs) = go (M.insert x (f ps x) acc) xs

minDistGrid :: [Coord] -> Map Coord (Maybe Id)
minDistGrid = mkGrid uniqueMin


totalDistGrid :: [Coord] -> Map Coord (Maybe Id)
totalDistGrid = mkGrid totalDist


uniqueMin :: (Num a, Enum a) => [Coord] -> (Int, Int) -> Maybe a
uniqueMin ps x = if length ms == 1 then Just (fst $ head ms) else Nothing
  where
    ds = manhattan x <$> ps
    ms = head $ groupBy (\x y -> snd x == snd y) $ sortBy (comparing snd) $ zip [0..] ds 


totalDist :: [Coord] -> (Int, Int) -> Maybe Int
totalDist ps x = if tot < 10000 then Just tot else Nothing
  where
    tot = sum $ manhattan x <$> ps


day6 :: IO ()
day6 = do
  inLines <- getLines 6
  let xs :: [Coord]
      xs = parseLine <$> inLines
      ((xl, yl), (xh, yh)) = gridPoints xs
      gd = minDistGrid xs
      edgeIds = nub $ catMaybes $ M.elems $ M.filterWithKey (\(x,y) _ -> (x == xl) || (x == xh) || (y == yl) || (y == yh)) gd
      notEdges = M.filter (\x -> isJust x && fromJust x `notElem` edgeIds) gd
      count = maximum $ (length <$>) <$> group $ sort $ catMaybes $ M.elems notEdges

  putStrLn $ "Day6: part1: " ++ show count
  putStrLn $ "Day6: part2: " ++ show (length $ catMaybes $ M.elems $ totalDistGrid xs)

  return ()


