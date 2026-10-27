{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}

module Day12 where


import Utils
import qualified Data.Map.Strict as M


type Pattern = String


type RuleMap = Map String Char


parseRules :: [String] -> RuleMap
parseRules s = M.fromList $ zip pns nxts
  where
    pns = take 5 <$> s
    nxts = last <$> s


-- A garden has an offset (ie. distance to the left most '#') and a pattern
newtype Garden = Garden (Int, String) deriving (Show)


-- Ignore the offset when comparing gardens
instance Eq Garden where
  Garden (_, g) == Garden (_, h) = g == h


instance Ord Garden where
  Garden (_, g) <= Garden (_, h) = g <= h  


apply :: RuleMap -> Garden -> Garden
apply rules (Garden (m, plants)) = Garden (m - 2 + length d, dropWhileEnd (=='.') t)
  where
    pplus = "...." ++ plants ++ "...."
    n = length plants
    ps = (\ix -> M.findWithDefault '.' (take 5 $ drop ix pplus) rules) <$> [0..(n+4)] 
    (d, t) = span (=='.') ps


score :: Garden -> Int
score (Garden (n, s)) = n*m + sum ((\(c, ix) -> if c=='#' then ix else 0) <$> zip s [0..(len-1)])
  where
    len = length s
    m = length $ filter (=='#') s


-- We add patterns to the pool so we can find a repeated pattern
type Pool = Map Garden Int


findRepeat :: RuleMap -> (Int, Garden, Pool) -> (Int, Garden, Pool)
findRepeat rules (n, g, p)
  | g `M.member` p = (n, g, p) -- ie. there is a repated pattern (maybe offset from the original)
  | otherwise = findRepeat rules (n+1, apply rules g, M.insert g n p)
    

day12 :: IO ()
day12 = do
  (topLine:inLines) <- getLines 12
  let rules :: RuleMap
      rules = parseRules $ tail inLines
      start = drop 15 topLine
      p1 = iterate (apply rules) (Garden (0, start)) !! 20
      (steadyState, g@(Garden (shift, ptn)), p) = findRepeat rules (0, Garden (0, start), M.empty)
      finalShift = 50000000000 - steadyState + shift

  putStrLn $ "Day12: part1: " ++ show (score p1)
  putStrLn $ "Day12: part2: " ++ show (score $ Garden (finalShift, ptn))

  return ()

