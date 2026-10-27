

module IGraph where

import Data.Graph.Inductive ( Graph(mkGraph), LEdge, LNode, Gr, sp, nodes, edges )
import qualified Data.Map as M
import Utils


makeGraph :: Map Coord [Coord] -> (Gr Coord Int, Coord -> Int, Int -> Coord)
makeGraph mp = (mkGraph ns es, (c2i M.!), (i2c M.!))
  where
    ns :: [LNode Coord]
    ns = zip [0..] $ M.keys mp

    es :: [LEdge Int]
    es = concatMap (\(c,cs) -> (\d -> (c2i M.! c, c2i M.! d, 1)) <$> cs) (M.toList mp)
    c2i :: Map Coord Int
    c2i = M.fromList $ zip (M.keys mp) [0..]
    i2c :: Map Int Coord
    i2c = M.fromList $ zip [0..] (M.keys mp)


shortestPath :: Coord -> Coord -> Map Coord [Coord] -> Maybe [Coord]
shortestPath start end ns = (i2c <$>) <$> sp (c2i start) (c2i end) gr
  where
    (gr, c2i, i2c) = makeGraph ns
    vs = sp (c2i start) (c2i end) gr


igraphMain :: IO ()
igraphMain = do
  let mp :: Map Coord [Coord]
      mp = M.fromList [((1,1), [(1,2), (2,1)]), ((1,2), [(1,3),(2,2)]), ((2,1), [(2,2), (3,1)]), ((3,1), [(4,1)]), ((1,3), [(1,4)]), ((1,4), []), ((4,1),[]), ((2,2), [])]
      (g, c2i, i2c) = makeGraph mp

  putStrLn $ "keys: " ++ show (M.keys mp)
  putStrLn $ "nodes: " ++ show (nodes g)
  putStrLn $ "edges: " ++ show (edges g)
  putStrLn $ "shortestPath: " ++ show (shortestPath (1,1) (1,4) mp)
  return ()