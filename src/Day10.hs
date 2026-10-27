
module Day10 where


import Utils hiding (span)


type Atom = (Coord, Coord)

parseLine :: String -> Atom
parseLine s = ((read sx, read sy), (read sdx, read sdy))
  where
    sx = take 6 $ drop 10 s
    sy = take 6 $ drop 18 s
    sdx = take 2 $ drop 36 s
    sdy = take 2 $ drop 40 s
    

move :: Int -> [Atom] -> [Atom]  
move n as = go <$> as
  where
    go (p, dp) = (p + (n,n) * dp, dp)  


area :: [Atom] -> Int
area as = (maximum xs - minimum xs) * (maximum ys - minimum ys)
  where
    xs = fst . fst <$> as
    ys = snd . fst <$> as


corners :: [Atom] -> (Coord, Coord)
corners as = ((minimum xs, minimum ys), (maximum xs, maximum ys))
  where
    xs = fst . fst <$> as
    ys = snd . fst <$> as


showPicture :: [Atom] -> String
showPicture as = intercalate "\n" $ transpose $ (\x -> (\y -> if (x, y) `elem` cs then '#' else '.') <$> [miny..maxy]) <$> [minx..maxx]
  where
    ((minx, miny), (maxx, maxy)) = corners as
    cs = fst <$> as


-- Calculate the minimum are using a slowed down Newton method
minimumArea :: Int -> [Atom] -> Int
minimumArea n as = snd $ go n 1000
  where
    accy = 10
    go :: Int -> Int -> (Int, Int)
    go n guess
      | abs da < accy = (ag, guess)
      | n == 0 = (ag, guess)
      | ag < aminus && ag <aplus = (ag, guess)
      | aminus < ag = go (n-1) $ round $ fromIntegral guess - abs (da / d2a / 4)
      | otherwise = go (n-1) $ round $ fromIntegral guess + abs (da / d2a / 4)
      where
        aminus = area $ move (guess-1) as
        ag = area $ move guess as
        aplus = area $ move (guess+1) as
        da, d2a :: Double
        da = fromIntegral (aplus - aminus)/2
        d2a = fromIntegral $ aplus - 2*ag + aminus


day10 :: IO ()
day10 = do
  inLines <- getLines 10
  let as :: [Atom]
      as = parseLine <$> inLines
      seconds = minimumArea 100 as
  putStrLn $ "Day10: part1:\n" ++ showPicture (move 10519 as)
  putStrLn $ "Day10: part2: " ++ show seconds
  return ()


