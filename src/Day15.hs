{-# LANGUAGE MultiWayIf #-}

module Day15 where


import Utils hiding (race, singleton, delete)
import Search
import HeapIndexed
import Data.Bimap (Bimap)
import qualified Data.Bimap as M
import Data.Functor.Sum (Sum(InL))
import Graph
import Data.Graph (dff)
import Data.Bool (Bool)
import Data.List.Split (endBy)
import Alg hiding (filter, zip, Label, length)
import qualified Data.Set as S
import Data.Function
import qualified Data.List.NonEmpty as NE
import System.TimeIt ( timeIt )


data Race = Elf | Gob deriving (Eq, Ord, Show)
data Unit = Unit {uid :: Int, race :: Race, hp :: Int, power :: Int} deriving (Eq)
type UnitMap = Bimap Coord Unit
data Game = Game { mp :: UnitMap, walls :: Set Coord, bounds :: Int, moves :: Int} deriving (Eq)


showCell :: Coord -> Game -> Char
showCell p g@(Game mp ws _ _) 
  | p `M.member` mp = if isElf (mp M.! p) then 'E' else 'G'
  | p `elem` ws = '#'
  | otherwise = '.'


isElf :: Unit -> Bool
isElf (Unit _ r _ _) = r == Elf


isGob :: Unit -> Bool
isGob (Unit _ r _ _) = r == Gob


instance Show Game where
  show :: Game -> String
  show g@(Game mp ws _ _) = intercalate "\n" $ (\y -> (\x -> showCell (x,y) g) <$> [0..31]) <$> [0..31]


instance Show Unit where
  show :: Unit -> String
  show (Unit id r hp p) = show id ++ ":" ++ show r ++ ":" ++ show hp ++ ":" ++ show p 


scoreGame :: Game -> Int
scoreGame g@(Game mp _ _ n) = n * sum (hp <$> M.keysR mp)


elfCount :: Game -> Int
elfCount g = length $ filter isElf $ M.keysR $ mp g


isFinished :: Game -> Bool
isFinished (Game mp _ _ _) = length g == 1
  where
    g = group $ sort $ race <$> M.keysR mp


-- For attacking fewest hitpoints is chosen and if a tie reading order of position
instance {-# OVERLAPPING #-} Ord Unit where
  (<=) :: Unit -> Unit -> Bool
  u1@(Unit id1 r1 hp1 p1) <= u2@(Unit id2 r2 hp2 p2)
    | hp1 == hp2 = id1 < id2
    | otherwise = hp1 <= hp2


move :: Game -> Game
move g 
  | isFinished g = g
  -- | n <= 0 = g
  | otherwise = move $ moveGame g 


moveGame :: Game -> Game
moveGame g@(Game mp _ _ _) = go g (M.keys mp)
  where
    go :: Game -> [Coord] -> Game
    go gg@(Game mp ws bs n) [] = Game mp ws bs (n+1)
    go gg (c:cs)
      | isFinished gg = gg
      | c `M.notMember` mp = go gg cs
      | isElf (mp M.! c) || isGob (mp M.! c) = go newG cs
      | otherwise = go gg cs
          where
            newG = moveUnit gg c
    

moveUnit :: Game -> Coord -> Game
moveUnit g@(Game mp ws bs n) pos 
  | pos `M.notMember` mp = g
  | M.null targets = g
  | mindist == 1 = attack g u
  | isNothing chosenRoute = g
  | otherwise = attack (Game (M.insert moveTo u $ M.delete pos mp) ws bs n) u
  where
    u@(Unit id r hp p) = mp M.! pos
    targets = M.filter (\_ w-> race w /= race u) mp
    mindist = minimum $ manhattan pos <$> M.keys targets
    chosenRoute = route pos u mp bs ws
    moveTo = fromJust chosenRoute


findTarget :: Unit -> UnitMap -> Maybe Unit
findTarget u mp
  | isNothing pos = Nothing
  | null possible = Nothing
  | otherwise     = Just $! mp M.! minimumBy ordering possible
  where
    pos :: Maybe Coord
    pos = mp M.!?> u
    ordering :: Coord -> Coord -> Ordering
    ordering = comparing (\i -> (hp (mp M.! i), i))
    possible :: [Coord]
    possible = filter (isEnemy . (mp M.!?)) $ neighbours4 $ fromJust pos
    isEnemy :: Maybe Unit -> Bool
    isEnemy Nothing = False
    isEnemy (Just w) = race w /= race u


attack :: Game -> Unit -> Game
attack g@(Game mp ws bs n) attacker
  | isNothing target = g
  | hp newTarget <=0 = Game (M.deleteR (fromJust target) mp) ws bs n
  | otherwise = Game (M.insert (mp M.!> fromJust target) newTarget mp) ws bs n
  where
    target = findTarget attacker mp
    newTarget :: Unit
    newTarget = Unit (uid $ fromJust target) (race $ fromJust target) (hp (fromJust target) - power attacker) (power $ fromJust target)



route :: Coord -> Unit -> UnitMap -> Int -> Set Coord -> Maybe Coord
route pos unit mp bs walls
  | isNear pos = Nothing
  | otherwise  = search S.empty candidates
  where
    candidates = S.fromList [(1, start, start) | start <- neighbours4 pos, not (isOut start) ]
    isOut loc@(lx, ly) = lx < 0 || lx >= bs || ly < 0 || ly >= bs || loc `S.member` walls || loc `M.member` mp
    isNear p = or (mapMaybe (isEnemy unit mp) $ neighbours4 p)

    search seen q =
      do ((dist, dest, start), newq) <- S.minView q
         if | S.member dest seen || isOut dest -> search seen newq
            | isNear dest -> Just start
            | otherwise -> search (S.insert dest seen)
                                  (foldl' (flip S.insert) newq [ (dist+1, next, start) | next <- neighbours4 dest, not (isOut next)])


isEnemy :: Unit -> UnitMap -> Coord -> Maybe Bool
isEnemy unit mp loc
  | isNothing u = Nothing
  | otherwise = Just $ race unit /= race (fromJust u)
  where
    u :: Maybe Unit
    u = mp M.!? loc


setElves :: Int -> Game -> Game
setElves p g@(Game mp ws bs n) = Game (M.mapR (\u@(Unit i r h _) -> if isElf u then Unit i r h p else u) mp) ws bs n


calcPower :: (Game, Int) -> Int
calcPower (g, p)
  | e1 == e0 = scoreGame g1
  | otherwise = calcPower (g, p + 1)
  where
    g1 = move $ setElves p g
    e0 = elfCount g
    e1 = elfCount g1


day15 :: IO ()
day15 = do
  ls <- getLines 15
  let g :: Game
      g = parseGame ls

  putStrLn $ "Day15: part1:" ++ show (scoreGame $ move g)
  putStrLn $ "Day15: part2: " ++ show (calcPower (g, 3))

  return ()


parseGame :: [String] -> Game
parseGame css = go (Game M.empty S.empty bs 0) 0 cs
  where
    bs = length css
    go :: Game -> Int -> [(Coord, Char)] -> Game
    go g _ [] = g
    go (Game mp ws bs n) ix ((p, c):ps) = go (help c) (ix + inc c) ps
      where
        help :: Char -> Game
        help 'E' = Game (M.insert p  (Unit ix Elf 200 3) mp) ws bs n
        help 'G' = Game (M.insert p (Unit ix Gob 200 3) mp) ws bs n
        help '.' = Game mp ws bs n
        help '#' = Game mp (p `S.insert` ws) bs n
        inc :: Char -> Int
        inc 'E' = 1
        inc 'G' = 1
        inc _ = 0
    

    cs :: [(Coord, Char)]
    cs = concatMap (\(y, cs) -> (\(x, c) -> ((x,y), c)) <$> zip [0..(nx-1)] cs) $ zip [0..(ny-1)] css
    ny = length css
    nx = length $ head css

