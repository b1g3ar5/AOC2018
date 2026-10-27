
module Day4 where

import Utils
import Data.List
import Data.List.Split
import Data.Ord
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Bifunctor

type Guard = Int

data Item = ShiftStart Guard | Sleep | Wake deriving (Show, Eq)


isStart :: Item -> Bool
isStart (ShiftStart _) = True
isStart _ = False

parseItem :: String -> Item
parseItem s
  | head ws == "Guard" = ShiftStart $ read $ tail $ ws!!1
  | head ws == "falls" = Sleep
  | head ws == "wakes" = Wake
  | otherwise = error $ "This shouldn't happen in parseItem: " ++ s
  where
    ws = words s

data Date = Date {mm :: Int, dd :: Int,  hr :: Int,  mn :: Int} deriving (Show, Eq, Ord)


mins :: Date -> Date -> Int
mins (Date m1 d1 h1 n1) (Date m2 d2 h2 n2) = ((d2-d1)*24 + (h2-h2))*60+(n2-n1)


parseDate :: String -> Date
parseDate s = Date (read $ dt!!1) (read $ dt!!2) (read $ head tm) (read $ tm!!1)
  where
    ws = words s
    dt = splitOn "-" $ tail $ head ws
    tm = splitOn ":" $ ws!!1


type Line = (Date, Item)


data Shift = Shift { guard :: Guard, sleep :: Int, wake :: Int}


parseLine :: String -> Line
parseLine s = (parseDate $ head ws, parseItem $ tail $ ws!!1)
  where
    ws = splitOn "]" s


mkSleeps :: [Line] -> [(Guard, (Date, Date))]
mkSleeps = go [] 0
  where
    go :: [(Int, (Date, Date))] -> Guard -> [Line] -> [(Guard, (Date, Date))]
    go ss _ [] = ss
    go ss gid ((d1, Sleep) : (d2, Wake) : xs) = go ((gid, (d1, d2)):ss) gid xs
    go ss _ ((d1, ShiftStart gid) : xs) = go ss gid xs
    go ss gid (x:xs) = error $ "did not parse in mkShift: " ++ show x


type SleepMap = Map Int Int

mkSleepMap :: [(Guard, (Date, Date))] -> SleepMap
mkSleepMap = go M.empty
  where
    go mp [] = mp
    go acc ((_, (s, e)):xs) = go (foldl (\mp m -> M.insertWith (+) m 1 mp) acc [(mn s)..(mn e-1)]) xs


getBestMinute :: [(Guard, (Date, Date))] -> (Int, (Int, Int))
getBestMinute xs@((gid, (_, _)):_) = (gid , maximumBy (comparing snd) $ M.toList $ mkSleepMap xs)
getBestMinute [] = error "No data in getBestMinute"

day4 :: IO ()
day4 = do
  inLines <- getLines 4
  let ls :: [Line]
      ls = sortOn fst $ parseLine <$> inLines
      toMins = second (uncurry mins)

      sleepList :: [(Guard, (Date, Date))]
      sleepList = mkSleeps ls
      
      -- I thought this would be more straight forward!!
      go :: Eq a => (a -> a -> Bool) -> (a, b) -> (a, b) -> Bool
      go f t1 t2 = uncurry f $ bimap fst fst $ curry id t1 t2

      allSleeps :: [(Guard, Int)]
      allSleeps = (\g -> (fst $ head g, sum $ snd <$> g)) <$> groupBy (go (==)) (sort $ toMins <$> sleepList)
      
      mostSleep :: (Guard, Int)
      mostSleep@(guard, sleep) = maximumBy (comparing snd) allSleeps
      
      guardSleeps :: [(Guard, (Date, Date))]
      guardSleeps = filter (\(g,(_,_)) -> g == guard) sleepList

      bestMinute1 = getBestMinute guardSleeps

      allMinutes = getBestMinute <$> groupBy (\(g, _) (h, _) -> g == h) (sortBy (\(g, _) (h, _) -> compare g h) sleepList)

      bestMinute2 = maximumBy (comparing (snd . snd)) allMinutes


  putStrLn $ "Day4: part1: " ++ show (fst mostSleep * fst bestMinute1)
  putStrLn $ "Day4: part2: " ++ show (fst bestMinute2 * fst (snd bestMinute2))
  
  return ()


