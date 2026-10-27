{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}


module Utils (
  module Utils
  , ord
  , comparing
  , module Data.List
  , splitOn
  , chunksOf
  , module Data.Bifunctor
  , module Data.Tuple
  , module Data.Maybe
  , module Control.Monad
  --, Set
  , module Data.Set
  --, Vector
  , module Data.Vector
  --, Map
  , module Data.Map.Strict
  , module Data.Sequence
  , trace
)
where


import Control.Monad.ST (runST, ST(..))
import System.TimeIt ( timeIt )

-- For reexporting
import Data.Char (ord)
import Data.Ord (comparing)
import Data.List
import Data.List.Split (splitOn, chunksOf)
import Data.Bifunctor
import Data.Tuple
import Data.Maybe
import Control.Monad
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed.Mutable as VM
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import qualified Data.IntMap.Strict as IM
import Data.Sequence (Seq(..), (><), (|>), (<|))
import qualified Data.Sequence as Seq
import qualified Data.List.NonEmpty as NE
import Debug.Trace (trace)


--- Things to add

-- Rectangular grid with focus and distributive, representable instances
-- Directions, rotations...

------------ GET THE INPUT FROM FILE ------------------

getF :: (String -> a) -> Int -> IO a
getF f n = do
  s <- readFile $ "./data/" ++ show n ++ ".in"
  return $ f s


getRaw :: Int -> IO String
getRaw = getF id


getWords :: Int -> IO [String]
getWords = getF words


getLines :: Int -> IO [String]
getLines = getF lines


getParagraphs :: Int -> IO [[String]]
getParagraphs = getF (filter (/=[]) . splitOnChar "" . lines)


------------------ VARIOUS UTILITY FUNCTIONS --------------------


intersections :: Ord a => [Set a] -> Set a
intersections ss = foldl' Set.intersection (head ss) (tail ss)


fix :: Eq a => (a -> a) -> a
fix f = x where x = f x


-- Should this just call fix somehow?
fixpoint :: Eq a => (a -> a) -> a -> a
fixpoint f x 
  | x ==fx = fx
  | otherwise = fixpoint f fx
  where
    fx = f x


-- This takes 2 predicates isFinished and isOK
-- and a start value and a next function
-- it returns True if isOK happens before isFinished
race :: (a -> Bool) -> (a -> Bool) -> a -> (a -> a) -> Bool
race isFinished isOk x next 
  | isFinished nxt = False
  | isOk nxt = True
  | otherwise = race isFinished isOk nxt next
  where
    nxt = next x


safe :: (Show k, Ord k) => M.Map k a -> String -> k -> a
safe mp msg k
  | k `M.member` mp = mp M.! k
  | otherwise = error $ "In: " ++ msg ++ " key:" ++ show k ++ " is not a member"


------------------- BOOLEAN / BINARY TO/FROM INT

-- This does conversion units at the front of the list
toInt :: [Bool] -> Integer
toInt [] = 0
toInt bs = 2 * toInt (tail bs) + if head bs then 1 else 0


fromInt :: Integer -> [Bool]
fromInt 0 = [False]
fromInt i = helper i
  where
    helper 0 = []
    helper i = let (q,r) = i `divMod` 2 in (r==1) : helper q


pad :: Integer -> a -> [a] -> [a]
pad n b bs = replicate (fromIntegral n - length bs) b ++ take (fromIntegral n) bs


------------------------ SPLITTING OF STRINGS -----------------------



-- Like words but you specify the character
splitOnChar :: Eq a => a -> [a] -> [[a]]
splitOnChar c = reverse . go []
  where
    go acc [] = acc
    go [] (x:xs)
      | x==c = go [] xs
      | otherwise = go [[x]] xs
    go acc@(w:ws) (x:xs)
      | x==c = go ([]:acc) xs
      | otherwise = go ((w++[x]):ws) xs


--splitOnStr :: Eq a => [a] -> [a] -> [[a]]
--splitOnStr = splitOn


------------------------ COORDINATE / VECTOR STUFF ------------

-- Traditionally measured down from top left
-- Coords are (x, y)

type Coord = (Int, Int)
type Coord3 = (Int, Int, Int)
type Route = NE.NonEmpty Coord

-- Reading order for Coord
--instance {-# OVERLAPPING #-} Ord Coord where
  --(x1,y1) <= (x2,y2) = if y1==y2 then x1<=x2 else y1<=y2


-- We choose shortest route or reading order on the initial coord
instance {-# OVERLAPPING #-} Ord Route where
  (<=) :: Route -> Route -> Bool
  a@(p NE.:| ps) <= b@(q NE.:| qs) = if length a == length b then 
                                        if p==q 
                                          then
                                            ps <= qs 
                                          else
                                            p < q 
                                        else 
                                          length a <= length b



instance Num Coord where
  (x1, y1) + (x2, y2) = (x1+x2, y1+y2)
  (x1, y1) - (x2, y2) = (x1-x2, y1-y2)
  (x1, y1) * (x2, y2) = (x1*x2, y1*y2)
  abs (x, y) = (abs x, abs y)
  signum (x, y) = (signum x, signum y)
  fromInteger i = (fromInteger i, 0)


manhattan :: Coord -> Coord -> Int
manhattan (x1, y1) (x2, y2) = abs (x1 - x2) +  abs (y1 - y2)

manhattan3 :: Coord3 -> Coord3 -> Int
manhattan3 (x1, y1, z1) (x2, y2, z2) = abs (x1 - x2) +  abs (y1 - y2) +  abs (z1 - z2)


euclidian :: Coord -> Coord -> Double
euclidian (x1, y1) (x2, y2) = sqrt $ fromIntegral ((x1 - x2)*(x1 - x2) + (y1 - y2)*(y1 - y2))


clockTurn :: Coord -> Coord
clockTurn (x, y) = (-y, x)
antiTurn :: Coord -> Coord
antiTurn (x, y) = (y, -x)


neighbourCoords8 :: [Coord]
neighbourCoords8 = [(x, y) | x <- [-1, 0, 1], y <- [-1, 0, 1], (x, y) /= (0, 0)]


-- in reading order
neighbourCoords4 :: [Coord]
neighbourCoords4 = [(0, -1), (-1, 0), (1, 0), (0, 1)]


neighbours8 :: Coord -> [Coord]
neighbours8 c = neighbourCoords8 `at` c

neighbours4 :: Coord -> [Coord]
neighbours4 c = neighbourCoords4 `at` c

isNeighbour8 :: Coord -> Coord -> Bool
isNeighbour8 a b = b `elem` neighbours8 a

isNeighbour4 :: Coord -> Coord -> Bool
isNeighbour4 a b = b `elem` neighbours4 a


at :: [Coord] -> Coord -> [Coord]
coords `at` origin = map (+ origin) coords


mul :: Int -> Coord -> Coord
mul c (x,y) = (c*x, c*y)


-- All coords in a grid in (x, y) (col, row) order
allCoords :: Int -> Int -> [Coord]
allCoords rows cols = concatMap (\c -> (c,) <$> [0..(rows-1)]) [0..(cols-1)]


directions8 :: [Coord]
directions8 = [(0, -1), (0, 1), (1, 0), (-1, 0), (1, -1), (1, 1), (-1, 1), (-1, -1)]

directions4 :: [Coord]
directions4 = [(0, -1), (0, 1), (1, 0), (-1, 0)]


-- Coordinate start at top left, so y goes up as you go down
lt, rt, up, dn :: Coord
lt = (-1,0)
rt = (1,0)
up = (0,-1)
dn = (0,1)

