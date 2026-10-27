{-# LANGUAGE TupleSections #-}

module Graph where

import Data.Graph hiding (Tree)
import Utils hiding (fix)
import qualified Data.Set as S
import Alg hiding (zip, filter, Tree)
import Data.Tree
import qualified Data.Map.Strict as M

{-
The grid is a list of coords so each coord is a node of the graph.
Each node is connected to the neighbours that are in the graph


-}

mkNodes :: Set Coord -> [(Coord, [Coord])]
mkNodes = go []
  where
    go acc s
      | null s = acc
      | otherwise = go ((c, filter (`elem` s) $ neighbours4 c):acc) rem
      where
        Just (c, rem) = S.minView s


scc :: [Coord] -> [SCC Coord]
scc cs = stronglyConnComp $ dup <$> mkNodes (S.fromList cs)
  where
    dup (x,y) = (x,x,y)


type GraphSet = (Graph, Vertex -> (Coord, Coord, [Coord]), Coord -> Maybe Vertex)

mkGraph :: Set Coord -> GraphSet
mkGraph cs = graphFromEdges $ dup <$> mkNodes cs
  where
    dup (x,y) = (x,x,y)


reachableCoords :: Coord -> GraphSet -> [Coord]
reachableCoords c (g, v2n, c2v) = cs
  where
    v = c2v c
    vs = reachable g $ fromMaybe (error "No vertex fot that coord") v
    cs = (\(c, _, _) -> c) . v2n <$> vs

{-
mkTree :: Coalgebra (TreeF (Int, Coord))  ((Int, Coord), Map Coord [Coord], [Coord])
mkTree ((n, c), mp, visited) = TreeF (n, c) $ zip ((n+1,) <$> ns) ms
  where
    ns = mp M.! c
    ms = reverse $ foldl (\(a:as) n -> del n a : a : as) [del c mp] ns
-}

-- Deletes the node of c and also the edges back to the nodes
-- that c links to
del :: Coord -> Map Coord [Coord] -> Map Coord [Coord]
del c mp = filter (`notElem` ls) <$> M.delete c mp
  where
    ls = mp M.! c


-- #####################################################

-- TREES FIRST

bfe :: Tree a -> [a]
bfe r = f r b []
  where
    f (Node x xs) fw bw = x : fw (xs : bw)
  
    b [] = []
    b qs = foldl (foldr f) b qs []


lwe' :: Tree a -> [[a]]
lwe' r = f b r [] []
  where
    f k (Node x xs) ls qs = k (x : ls) (xs : qs)

    b _ [] = []
    b k qs = k : foldl (foldl f) b qs [] []


lwe :: Tree a -> [[a]]
lwe r = go r []
  where
    go (Node x xs) (q:qs) = (x:q) : foldr go qs xs
    go (Node x xs) []     = [x]   : foldr go [] xs


-- NOW GRAPHS #################################
type Graph' a = a -> [a]


-- mark off nodes as we find them, with some set structure:
bfs' :: Ord a => Graph' a -> Graph' a
bfs' g = \z -> go z b [] S.empty
  where
    -- go :: Ord a => a -> ([[a]] -> Set a -> [a]) -> [[a]] -> Set a -> [a]
    go x fw bw s
      | S.member x s = fw bw s
      | otherwise      = x : fw (g x : bw) (S.insert x s)

    --b :: [[a]] -> Set a -> [a]
    b [] _ = []
    b qs s = foldl (foldr go) b qs [] s


bfs :: Ord a => Nodes a -> Nodes a
bfs g = M.fromList $ (\k -> (k, mpfn k)) <$> M.keys g
  where
    mpfn = \z -> go z b [] S.empty
    --go :: Ord a => a -> ([[a]] -> Set a -> [a]) -> [[a]] -> Set a -> [a]
    go x fw bw s
      | S.member x s = fw bw s
      | otherwise      = x : fw (M.findWithDefault [] x g : bw) (S.insert x s)

    --b :: [[a]] -> Set a -> [a]
    b [] _ = []
    b qs s = foldl (foldr go) b qs [] s



-- The levelwise algorithm is similar:
lws1 :: Ord a => Graph' a -> a -> [[a]] 
lws1 g r = f b r [] [] S.empty
  where
    f k x ls qs s
      | S.member x s = k ls qs s
      | otherwise = k (x : ls) (g x : qs) (S.insert x s)

    b _ [] _ = []
    b k qs s = k : foldl (foldl f) b qs [] [] s


lws2 :: Graph' a -> a -> [[a]]
lws2 g r = go r []
  where
    go x (q:qs) = (x:q) : foldr go qs (g x)
    go x []     = [x]   : foldr go [] (g x)


lws' :: Ord a => Graph' a -> a -> [[a]]
lws' g r = takeWhile (not.null) (map fst (fix (f r . push)))
  where
    push xs = ([],S.empty) : [([],seen) | (_,seen) <- xs ]
    f _ [] = error "Shouldn't be possible in lws'"
    f x q@((l,s):qs)
      | S.member x s = q
      | otherwise = (x:l, S.insert x s) : foldr f qs (g x)


type Nodes a = Map a [a]

-- Returns the nodes in each level (ie. steps) from start
lws :: Ord a => a -> Nodes a -> [[a]]
lws start mp = takeWhile (not.null) (fst <$> fix (go start . push))
  where
    push :: [([a], Set a)] ->  [([a], Set a)]
    push xs = ([],S.empty) : [([],seen) | (_,seen) <- xs ]
    --go :: Ord a => a -> [([a], Set a)] -> [([a], Set a)]
    go _ [] = error "Shouldn't be possible in lws"
    go x q@((l,s):qs)
      | S.member x s = q
      | otherwise = (x:l, S.insert x s) : foldr go qs (M.findWithDefault [] x mp)


