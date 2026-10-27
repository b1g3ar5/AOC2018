
module Day22 where


import Utils
import Data.Map qualified as M
import Data.Set qualified as S

import HeapIndexed qualified as H


type Geo = Int
type Erosion = Int
type Mins = Int

data Type = Rocky | Wet | Narrow deriving (Show, Eq, Ord)
type Cave = Map Coord (Geo, Erosion, Type)
data Tool = Torch | Climbing | Neither deriving (Show, Eq, Ord)

tools = [Torch, Climbing, Neither]

type State = (Coord, Tool)


depth, targetx, targety :: Int
depth = 3339
targetx = 10
targety = 715


cavex, cavey :: Int
cavex = 50
cavey = 750

buildCave :: Cave -> Cave
buildCave cave = foldl' (\c (x', y') -> M.insert (x', y') (geoErosionType (x', y') c) c) cave 
    [(x', y') | x' <- [0..cavex], y' <- [0..cavey]]
  where
    geoErosionType :: Coord -> Cave -> (Geo, Erosion, Type)
    geoErosionType (x', y') c = (geo, erosion, typ)
      where
        geo = case (x', y') of
          (0, 0) -> 0
          (x', y') | x' == targetx && y' == targety -> 0
          (x, 0) -> x * 16807
          (0, y) -> y * 48271
          _ -> let (_, eroL, _) = c M.! (x'-1, y')
                   (_, eroU, _) = c M.! (x', y'-1)
               in eroL * eroU
        erosion = (geo + depth) `mod` 20183
        typ = case erosion `mod` 3 of
          0 -> Rocky
          1 -> Wet
          2 -> Narrow 


disAllowedTool :: Cave -> Coord -> Tool
disAllowedTool cave pos = case typ of
  Rocky -> Neither --[Torch, Climbing]
  Wet -> Torch --[Climbing, Neither]
  Narrow -> Climbing --[Torch, Neither]
  where
    (_, _, typ) = cave M.! pos


moves :: Cave -> State -> [State]
moves cave (pos, tool) = [(pos', tool') | pos'@(xc, yc) <- neighbours4 pos, xc<cavex, yc<cavey, xc>=0, yc>=0
                                        , tool' <- tools , tool' /= disAllowedTool cave pos', tool' /= disAllowedTool cave pos]


cost :: State -> State -> Mins
cost (pos1, tool1) (pos2, tool2) =  if tool1 == tool2 then 1 else 8


dijkstra :: H.Heap Mins State -> S.Set State -> (State -> [State]) -> (State -> Bool) -> Maybe Mins
dijkstra pipeline visited movesFn finished
  | H.isEmpty pipeline = Nothing
  | finished state = Just savedMin
  | state `S.member` visited = dijkstra remainingPipeline visited movesFn finished
  | otherwise = dijkstra newPipeline (S.insert state visited) movesFn finished
  where
    ((savedMin, state), remainingPipeline) = fromJust $ H.extractMin pipeline
    newStates = filter (`S.notMember` visited) $ movesFn state
    newPipeline = remainingPipeline `H.union` H.fromList ((\n -> (savedMin + cost state n, n)) <$> newStates)


day22 :: IO ()
day22 = do
  inWords <- getWords 22
  let cave :: Cave
      cave = buildCave M.empty
      risk = sum [case typ of
                Rocky -> 0
                Wet -> 1
                Narrow -> 2 | ((x, y), (_, _, typ)) <- M.toList cave, x <= targetx, y <= targety]
      time = dijkstra (H.singleton 0 ((0, 0), Torch)) S.empty (moves cave) (\(c, t) -> c == (targetx, targety) && (t == Torch))

  putStrLn $ "Day22: part1: " ++ show risk
  putStrLn $ "Day22: part2: " ++ show time

  return ()


