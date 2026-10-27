{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module Day25 where


import Utils ( groupBy, sort, splitOn, Set, getLines )
import Data.Set qualified as S


pattern Empty :: S.Set a
pattern Empty <- (S.null -> True)

pattern (:|) :: Ord a => a -> S.Set a -> S.Set a
pattern x :| rest <- (S.minView -> Just (x, rest))


type Coord4 = (Int, Int, Int, Int)

man4 :: Coord4 -> Coord4 -> Int
man4 (x1,x2,x3,x4) (y1,y2,y3,y4) = abs (x1-y1) + abs (x2-y2) + abs (x3-y3) + abs (x4-y4)

--parseCoord4 :: String -> Coord4
parseCoord4 s = (read $ ws!!0, read $ ws!!1, read $ ws!!2, read $ ws!!3)
  where
    ws = splitOn "," s


closeTest :: Coord4 -> Coord4 -> Bool
closeTest x =  (<= 3) . man4 x

constellations :: Set Coord4 -> [Set Coord4]
constellations Empty = []
constellations (p :| ps) = filter (/= S.empty) $ newCs : constellations finishRem
  where
    (newCs, finishRem) = go (S.singleton p) S.empty ps
    go :: Set Coord4 -> Set Coord4 -> Set Coord4 -> (Set Coord4, Set Coord4)
    go pipeline acc Empty = (acc `S.union` pipeline, S.empty)
    go Empty acc fodder = (acc, fodder)
    go (p :| pipeline) acc fodder = go (pipeline `S.union` close) (p `S.insert` acc) far
      where
        (close, far) = S.partition (closeTest p) fodder


day25 :: IO ()
day25 = do
  inLines <- getLines 25
  let xs :: Set Coord4
      xs = S.fromList $ parseCoord4 <$> inLines
      cs = constellations xs
  putStrLn $ "Day25: part1: " ++ show (length cs)

  return ()



test = ["1,-1,-1,-2"
    , "-2,-2,0,1"
    , "0,2,1,3"
    , "-2,3,-2,1"
    , "0,2,3,-2"
    , "-1,-1,1,-2"
    , "0,-2,-1,0"
    , "-2,2,3,-1"
    , "1,2,2,0"
    , "-1,-2,0,-2"]

test1 = ["1,-1,0,1"
    , "2,0,-1,0"
    , "3,2,-1,0"
    , "0,0,3,1"
    , "0,0,-1,-1"
    , "2,3,-2,0"
    , "-2,2,0,0"
    , "2,-2,0,-1"
    , "1,-1,0,-1"
    , "3,2,0,2"]