
module Day2 where


import Utils
import Data.List
import Data.Set (Set)
import qualified Data.Set as S
import Data.Maybe

twoThree :: String -> (Bool, Bool)
twoThree xs = (2 `elem` gs, 3 `elem` gs)
  where
    gs = length <$> group (sort xs)


newtype ID a = ID [a] deriving (Show)


instance (Eq a, Ord a) => Eq (ID a) where
  ID x == ID y = length (diff x y) == 1


instance (Eq a, Ord a) => Ord (ID a) where
  ID x <= ID y = x <= y || length (diff x y) == 1


diff :: Eq a => [a] -> [a] -> [a]
diff xs ys
  | length xs > length ys = xs \\ ys
  | otherwise = ys \\ xs


findIDs :: [String] -> String
findIDs = go S.empty 
  where
    go :: Set (ID Char) -> [String] -> String
    go acc [] = error "Couldn't find ID"
    go acc (x:xs)
      | xid `S.member` acc = filter (`elem` y) x
      | otherwise = go (S.insert xid acc) xs
      where
        xid = ID x
        (ID y) = fromMaybe (error $ "This shouldn't be empty: " ++ x) $ xid `S.lookupLE` acc


day2 :: IO ()
day2 = do
  inLines <- getLines 2
  let xs :: [String]
      xs = inLines
      (twos, threes) = unzip $ twoThree <$> xs
      go bs = length $ filter id bs
  putStrLn $ "Day2: part1: " ++ show (go twos * go threes)
  putStrLn $ "Day2: part2: " ++ findIDs xs

  return ()


