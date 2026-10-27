
module Day7 where


import Utils


parseLine :: [Char] -> (Char, Char)
parseLine s = (head $ ws!!1, head $ ws!!7)
  where
    ws = words s


findIndependent :: [(Char, Char)] -> String 
findIndependent ccs = sort $ indept \\ dept
  where
    dept = nub $ sort $ snd <$> ccs
    indept = nub $ sort $ fst <$> ccs


workList :: [(Char, Char)] -> String
workList = go []
  where
    go :: String -> [(Char, Char)] -> String
    go acc [] = acc
    go acc [(b,a)] = acc ++ [b, a]
    go acc todo = go (acc ++ [next]) $ filter (\(b,a)-> b /= next) todo
      where
        (next:ts) = findIndependent todo


taskTime :: Char -> Int
taskTime c = ord c - ord 'A' + 61


timeRequired :: [(Char, Char)] -> Int
timeRequired = go 0 [] []
  where
    go :: Int -> String -> [(Char, Int)] -> [(Char, Char)] -> Int
    go timeSoFar done [] [] = timeSoFar
    go timeSoFar done inProgress todo 
      -- No independent processes so finish the fastest inProgress one
      | null indept = go (timeSoFar + mnTime) (mnTask:done) stillInProgress remaining
      -- Not all workers working and there are tasks to start...
      | length inProgress < 5 = go timeSoFar done (inProgress ++ [(head indept, taskTime $ head indept)]) todo
      -- No workers available so finish the fastest inProgress one
      | otherwise = go (timeSoFar + mnTime) (mnTask:done) stillInProgress remaining
      where
        indept = filter (`notElem` (fst <$> inProgress)) $ findIndependent todo
        remaining
          -- Add a dummy task (*) for the only dependent last task.
          | length todo == 1 = [(\ (b, a) -> (a, '*')) $ head todo | snd (head todo) /= '*']
          | otherwise = filter (\(b, _) -> b /= mnTask) todo
        (mnTask, mnTime) = minimumBy (comparing snd) inProgress
        stillInProgress = (\(x, tm) -> (x, tm - mnTime)) <$> filter (\(t, _) -> t /= mnTask) inProgress

day7 :: IO ()
day7 = do
  inLines <- getLines 7
  let xs :: [(Char, Char)]

      xs = parseLine <$> inLines
  putStrLn $ "Day7: part1: " ++ show (workList xs)
  putStrLn $ "Day7: part2: " ++ show (timeRequired xs)

  return ()


