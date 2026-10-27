
module Day13 where


import Utils
import qualified Data.Map.Strict as M
import Debug.Trace


data Track = V | H | U | D | C deriving (Show)

type Grid = Map Coord Track

parseTrack :: Coord -> Char -> Maybe (Coord, Track)
parseTrack p '/' = Just (p, U)
parseTrack p '\\' = Just (p, D)
parseTrack p '|' = Just (p, V)
parseTrack p '-' = Just (p, H)
parseTrack p '+' = Just (p, C)
parseTrack p '>' = Just (p, H)
parseTrack p '<' = Just (p, H)
parseTrack p '^' = Just (p, V)
parseTrack p 'v' = Just (p, V)
parseTrack p _ = Nothing


parseTruck :: Coord -> Char -> Maybe Truck
parseTruck p '>' = Just (Truck p rt TL)
parseTruck p '<' = Just (Truck p lt TL)
parseTruck p '^' = Just (Truck p up TL)
parseTruck p 'v' = Just (Truck p dn TL)
parseTruck p _ = Nothing


data Turn = TL | TS | TR deriving (Show, Eq, Ord, Enum)


-- Change in position = up, dn, lt, rt
type Vel = Coord


nextTurn :: Turn -> Turn
nextTurn TL = TS
nextTurn TS = TR
nextTurn TR = TL


applyTurn :: Turn -> Vel -> Vel
applyTurn TL = antiTurn
applyTurn TS = id
applyTurn TR = clockTurn


-- Reflect the velocity in the x=y line (for the slashes)
reflectXY, reflectXY' :: Vel -> Vel
reflectXY (x, y) = (y, x)
reflectXY' (x, y) = (-y, -x)


data Truck = Truck {pos :: Coord, dpos :: Vel, nxt :: Turn} deriving (Show, Eq)


instance Ord Truck where
  (Truck (x1, y1) _ _) <= (Truck (x2, y2) _ _) = 
    if y1==y2 then x1<=x2 else y1<=y2



parseGrid :: [String] -> (Grid, [Truck])
parseGrid xss = (M.fromList $ catMaybes tracks, catMaybes trucks)
  where
    tracks = concatMap (\(y, xs) -> (\(x, c) -> parseTrack (x, y) c) <$> zip [0..] xs) $ zip [0..] xss
    trucks = concatMap (\(y, xs) -> (\(x, c) -> parseTruck (x, y) c) <$> zip [0..] xs) $ zip [0..] xss


move :: Grid -> [Truck] -> Coord
move g = go []
  where
    go moved [] = go [] $ sort moved
    go moved ((Truck p dp nx):ts)
      | p + dp `elem` truckPositions = p+dp
      | otherwise = go (movedTruck:moved) ts
      where
        truckPositions = pos <$> moved ++ ts
        movedTruck = case g M.! (p + dp) of
                       V -> Truck (p+dp) dp nx
                       H -> Truck (p+dp) dp nx
                       U -> Truck (p+dp) (reflectXY' dp) nx
                       D -> Truck (p+dp) (reflectXY dp) nx
                       C -> Truck (p+dp) (applyTurn nx dp) (nextTurn nx)


remove :: Coord -> [Truck] -> [Truck]
remove p = filter (\(Truck q _ _) -> q /= p)


move2 :: Grid -> [Truck] -> Truck
move2 g = go []
  where
    -- Stop when there's only one truck left
    go [t] [] = t
    -- When tick has finished, sort them out and go again
    go moved [] = go [] $ sort moved
    go moved ((Truck p dp nx):ts)
      | p + dp `elem` truckPositions = go (remove (p+dp) moved) (remove (p + dp) ts)
      | otherwise = go (movedTruck:moved) ts
      where
        truckPositions = pos <$> (moved ++ ts)
        movedTruck = case g M.! (p+dp) of
                       V -> Truck (p+dp) dp nx
                       H -> Truck (p+dp) dp nx
                       U -> Truck (p+dp) (reflectXY' dp) nx
                       D -> Truck (p+dp) (reflectXY dp) nx
                       C -> Truck (p+dp) (applyTurn nx dp) (nextTurn nx)


day13 :: IO ()
day13 = do
  inLines <- getLines 13
  let (g, ts) = parseGrid inLines

  putStrLn $ "Day13: part1: " ++ show (move g ts)
  putStrLn $ "Day13: part2: " ++ show (pos $ move2 g $ sort ts)
  
  return ()



test = [
    "/->-\\        "
  , "|   |  /----\\"
  , "| /-+--+-\\  |"
  , "| | |  | v  |"
  , "\\-+-/  \\-+--/"
    , "\\------/"]

test2 = ["/>-<\\  "
  , "|   |  "
  , "| /<+-\\"
  , "| | | v"
  , "\\>+</ |"
  , "  |   ^"
  , "  \\<->/"]