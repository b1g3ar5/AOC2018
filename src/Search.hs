module Search where

import Utils (Set, fromJust, Coord(..), neighbours4, Route(..))
import HeapIndexed (Heap, singleton, union, fromList, extractMin, isEmpty)
import qualified Data.Set as S
import qualified Data.IntMap as IM
import qualified Data.List.NonEmpty as NE
import Debug.Trace


type State = (Coord, [Coord])
type State' = (Route, [Coord])

type Cost = Int
instance Semigroup Cost where
  c <> d = c+d

instance Monoid Cost where
  mempty = 0


moves :: State -> [State]
moves (c, cs) = (,cs) <$> filter (`elem` cs) (neighbours4 c)


moves' :: State' -> [State']
moves' (path, cs) = (\p -> (path <> (p NE.:| []),cs)) <$> filter (\c -> c `elem` cs && c `notElem` path) (neighbours4 $ NE.last path)


cost :: s -> s -> Cost
cost _ _  = 1


-- Path length
--dijkstra :: Heap Int State -> Set State -> (State -> Bool) -> Maybe Cost
dijkstra :: Ord s => Heap Int s -> Set s -> (s -> [s]) -> (s -> s -> Cost) -> (s -> Bool) -> Maybe Cost
dijkstra pipeline visited movesFn costFn finished
  | isEmpty pipeline = Nothing
  | finished state = Just savedMin
  | state `S.member` visited = dijkstra remainingPipeline visited movesFn costFn finished
  | otherwise = dijkstra newPipeline (S.insert state visited) movesFn costFn finished
  where
    ((savedMin, state), remainingPipeline) = fromJust $ extractMin pipeline
    newStates = filter (`S.notMember` visited) $ movesFn state
    newPipeline = remainingPipeline `union` fromList ((\n -> (savedMin <> cost state n, n)) <$> newStates)


-- Path
dijkstra' :: Heap Route State' -> Set State' -> (State' -> Bool) -> Maybe Route
dijkstra' pipeline visited finished
  | isEmpty pipeline = Nothing
  | finished state = Just savedMin
  | state `S.member` visited = dijkstra' remainingPipeline visited finished
  | otherwise = dijkstra' newPipeline (S.insert state visited) finished
  where
    ((savedMin, state), remainingPipeline) = fromJust $ extractMin pipeline
    newStates = filter (`S.notMember` visited) $ moves' state
    newPipeline = remainingPipeline `union` fromList ((\n -> (fst n, n)) <$> newStates)




