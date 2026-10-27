{-# language TypeFamilies #-}
module Conway
  ( mkGrid
  , basicRule
  , step
  , render
  , at
  , Grid
  , Rule
  , score
  ) where

import Data.Functor.Compose (Compose(..))
import Data.Vector (Vector, (!), generate)
import Data.Bool (bool)
import Data.Distributive (Distributive(..))
import Data.Functor.Rep (Representable(..), distributeRep)
import Data.Functor.Identity (Identity(..))
import Control.Comonad.Representable.Store (Store(..), StoreT(..), store, experiment, pos)
import Control.Comonad (Comonad(..))
import Data.List (group, sort)
import Debug.Trace


data Cell = Tree | Lumber | Open deriving (Eq, Show)
type Coord = (Int, Int)
type Grid = Store (Compose Vector Vector) Cell
type Rule = Grid -> Cell

instance Distributive Vector where
  distribute = distributeRep

instance Representable Vector where
  type Rep Vector = Int
  index v i = v ! (i `mod` gridSize)
  tabulate = generate gridSize

gridSize :: Int
gridSize = 50 --10

neighbourCoords :: [Coord]
neighbourCoords = [(x, y) | x <- [-1, 0, 1], y <- [-1, 0, 1], (x, y) /= (0, 0)]

addCoords :: Coord -> Coord -> Coord
addCoords (x, y) (x', y') = (x + x', y + y')

basicRule :: Rule
basicRule g 
  -- | cellType == Tree = trace ("Tree to lumber at: " ++ show (pos g)) $ if numNeighboursLumber >= 3 then Lumber else Tree
  | cellType == Tree = if numNeighboursLumber >= 3 then Lumber else Tree
  | cellType == Lumber = if (numNeighboursLumber >= 1) && (numNeighboursTree >= 1) then Lumber else Open
  | cellType == Open = if numNeighboursTree >= 3 then Tree else Open
  where
    cellType = extract g
    neighbours = experiment (at neighbourCoords) g
    numNeighboursTree = length (filter (==Tree) neighbours)
    numNeighboursLumber = length (filter (==Lumber) neighbours)
    --numNeighboursOpen = 8 - numNeighboursLumber - numNeighboursTree

step :: Rule -> Grid -> Grid
step = extend

render :: Grid -> String
render (StoreT (Identity (Compose g)) _) = foldMap ((++ "\n") . foldMap (\c -> if c==Tree then "|" else if c==Lumber then "#" else ".")) g


--score :: Grid -> Int
score g = length (ss!!0) * length (ss!!2)
    where
      ss = group $ sort $ concat $ lines $ render g

mkGrid :: ([Coord], [Coord]) -> Grid
mkGrid (ts, ls) = store (\c -> if c `elem` ls then Lumber else 
                                if c `elem` ts then Tree
                                else Open) (0, 0)

at :: [Coord] -> Coord -> [Coord]
coords `at` origin = filter (\(x,y) -> x>=0 && y>=0 && x<gridSize && y<gridSize) $ map (addCoords origin) coords

{-

0.......##.
1......|###
2.|..|...#.
3..|#||...#
4..##||.|#|
5...#||||..
6||...|||..
7|||||.||.|
8||||||||||
9....||..|.

.......##.
......|###
.|..|...#.
..|#||...#
..##||.|#|
...#||||..
||...|||..
|||||.||.|
||||||||||
....||..|.

-}