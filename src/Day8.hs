
module Day8 where


import Utils hiding (lookup)
--import Data.Tree
--import Data.List
import Data.Bifunctor
import Alg  (Tree(..), TreeF(..), ListF(..), Fix(..), cata, Coalgebra(..), ana)



-- Not quite a coalgrebra - we need the leftovers to do the nodes
-- The tree is made from an [Int]
-- The nodes (which are trees) are made from the leftover [Int] from the previous node
-- Now I took out the extra returned item, but now I have the actual nodes not the
-- [Int]'s that the nodes can be made from (using ana)
parseItem :: [Int] -> Tree [Int]
parseItem = fst . parseItemWithLeftovers


parseItemWithLeftovers :: [Int] -> (Tree [Int], [Int])
parseItemWithLeftovers [] = error "This shouldn't be possiblle in parseInput"
parseItemWithLeftovers [x] = error "This shouldn't be possiblle in parseInput"
parseItemWithLeftovers (nNodes:nEntries:xs) = (Fix (TreeF entries nodes), leftover)
  where
    (nodes, rem) = foldl (\(ts, lo) _ -> first (\x -> ts ++ [x]) $ parseItemWithLeftovers lo) ([],xs) [0..(nNodes-1)]
    (entries, leftover) = splitAt nEntries rem


sumEntries :: TreeF [Int] Int -> Int
sumEntries (TreeF es ns) = sum es + sum ns

showTree :: TreeF [Int] String -> String 
showTree (TreeF es ns) = show es ++ intercalate ", " ns


sum2 :: TreeF [Int] Int -> Int
sum2 (TreeF es ns)
  | null ns = sum es
  | otherwise = sum $  (\ix -> ns `safeLookup` (ix-1)) <$> es


safeLookup :: [Int] -> Int -> Int
safeLookup xs ix 
  | ix < 0 = 0
  | ix >= length xs = 0
  | otherwise = xs !! ix


test = words "2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2"

day8 :: IO ()
day8 = do
  inWords <- getWords 8
  let xs :: [Int]
      xs = read <$> inWords
      t = parseItem xs

  --putStrLn $ "Day8: tree: " ++ cata showTree t
  putStrLn $ "Day8: part1: " ++ show (cata sumEntries t)
  putStrLn $ "Day8: part2: " ++ show (cata sum2 t)
  


  return ()


