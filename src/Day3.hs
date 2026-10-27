{-# LANGUAGE BangPatterns #-}

module Day3 where


import Utils (splitOn, Coord, getLines)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Set qualified as S
import Data.List ((\\), nub)


data Rect = Rect { rid :: Int, pos :: Coord, sz :: Coord} deriving (Show)


toCoords :: Rect -> S.Set Coord
toCoords (Rect _ (x,y) (w,h)) = S.fromList $ [(x + dx, y + dy) | dx <- [0..(w-1)], dy <- [0..(h-1)]]


parseLine :: String -> Rect
parseLine s = Rect (read $ tail $ head ws) (read $ head xy, read $ init $ xy!!1) (read $ head wh, read $ wh!!1)
  where
    ws = words s
    xy = splitOn "," $ ws!!2
    wh = splitOn "x" $ ws!!3


type Grid = Map Coord (S.Set Int)


addRect :: Grid -> Rect -> Grid
addRect g r@(Rect rid _ _) = go g  $ toCoords r
  where
    --go g [] = g
    --go g (c:cs) = go (M.insertWith (S.insert) c rid g) cs
    go :: Grid -> S.Set Coord -> Grid
    go g s
      | S.null s = g
      | otherwise = go (M.insertWith (S.union) c (S.singleton rid) g) cs
      where
        (c,cs) = S.deleteFindMin s


mkGrid :: [Rect] -> Grid
mkGrid = go M.empty
  where
    go acc [] = acc
    go acc (x:xs) = go (addRect acc x) xs


day3 :: IO ()
day3 = do
  inLines <- getLines 3
  let xs :: [Rect]
      xs = parseLine <$> inLines
      g = mkGrid xs
  
  putStrLn $ "Day3: part1: " ++ show (M.size $ M.filter (\l -> length l >1) g)
  putStrLn $ "Day3: part2: " ++ show (S.fromList [1..1293] `S.difference` (S.unions $ M.elems $ M.filter (\s -> (S.size s) > 1) g))
  
  return ()

