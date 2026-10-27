
module Day23 where

import Utils
import Data.List.NonEmpty (sortWith)
import Data.Bits (Bits(bitSize))


type Bot = (Int, Coord3)


parseBot :: String -> Bot
parseBot s = (read $ drop 3 $ last $ ws, (read $ ws!!0, read $ ws!!1, read $ init $ ws!!2))
  where
    ws = splitOn "," $ drop 5 s


inrange :: Bot -> [Bot] -> [Bot]
inrange (cr, cpos) = filter (\(_, pos) -> manhattan3 cpos pos <= cr)


countInRange :: Coord3 -> [Bot] -> Int
countInRange = countInRangeWithMargin 0

countInRangeWithMargin :: Int -> Coord3 -> [Bot] -> Int
countInRangeWithMargin margin c bs = length $ filter (\(rng, pos) -> manhattan3 c pos <= (rng + margin)) bs


findBounds :: [Bot] -> (Coord3, Coord3)
findBounds bs = (foldl1 min3 $ map snd bs, foldl1 max3 $ map snd bs)
  where
    min3 (x1,y1,z1) (x2,y2,z2) = (min x1 x2, min y1 y2, min z1 z2)
    max3 (x1,y1,z1) (x2,y2,z2) = (max x1 x2, max y1 y2, max z1 z2)  


listCoords :: Coord3 -> Coord3 -> Int -> [Coord3]
listCoords (x1,y1,z1) (x2,y2,z2) step = [(x,y,z) | x <- [x1,(x1+step)..x2], y <- [y1,(y1+step)..y2], z <- [z1,(z1+step)..z2]]


maxByCount :: (Int, Coord3) -> (Int, Coord3) -> (Int, Coord3)
maxByCount (count1, c1@(x1, y1, z1)) (count2, c2@(x2, y2, z2))
  | count1 > count2 = (count1, c1)
  | count2 > count1 = (count2, c2)
  | otherwise = if (x1+y1+z1) <= (x2+y2+z2) then (count1, c1) else (count2, c2)
        

findPoint :: [Bot] -> Int
findPoint bs = go (2^24) $ findBounds bs
  where   
    go step (minC, maxC) = if step <= 1 then (bx+by+bz) else go (step `div` 2) (newMinC, newMaxC)
      where
        margin = 2*(step-1)
        counts = [(countInRangeWithMargin margin c bs, c) | c <- listCoords minC maxC step]
        (bestCount, best@(bx, by, bz)) = foldl1 maxByCount counts
        newMinC = (bx - margin, by - margin, bz - margin)
        newMaxC = (bx + margin, by + margin, bz + margin)


day23 :: IO ()
day23 = do
  inLines <- getLines 23
  let bs :: [Bot] 
      bs = sortOn (negate . fst) $ parseBot <$> inLines
      strongest@(sr, spos) = head $ bs
      bounds = findBounds bs

  putStrLn $ "Day23: part1: " ++ show (length $ inrange strongest bs)
  putStrLn $ "Day23: part2: " ++ show (findPoint bs)
  return ()


test = ["pos=<0,0,0>, r=4"
    , "pos=<1,0,0>, r=1" 
    , "pos=<4,0,0>, r=3"
    , "pos=<0,2,0>, r=1"
    , "pos=<0,5,0>, r=3"
    , "pos=<0,0,3>, r=1"
    , "pos=<1,1,1>, r=1"
    , "pos=<1,1,2>, r=1"
    , "pos=<1,3,1>, r=1"]


test2 = ["pos=<10,12,12>, r=2"
    , "pos=<12,14,12>, r=2"
    , "pos=<16,12,12>, r=4"
    , "pos=<14,14,14>, r=6"
    , "pos=<50,50,50>, r=200"
    , "pos=<10,10,10>, r=5"]