
module Day1 where


import Utils
import Data.Set (Set)
import qualified Data.Set as S


firstRepeat :: [Int] -> Int
firstRepeat xs = go S.empty 0 $ concat $ repeat xs
  where
    go :: Set Int -> Int -> [Int] -> Int
    go _ _ [] = error "This shouldn't happen in go"
    go acc sm (x:xs)
      | newSm `S.member` acc = newSm
      | otherwise = go (S.insert newSm acc) newSm xs
      where
        newSm = sm + x


day1 :: IO ()
day1 = do
  inLines <- getLines 1
  let xs :: [Int]
      xs = read . filter (/='+') <$> inLines
  putStrLn $ "Day1: part1: " ++ show (sum xs)
  putStrLn $ "Day1: part2: " ++ show (firstRepeat xs)

  return ()


