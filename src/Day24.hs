
module Day24 where


import Data.Ord
import Utils (maximumBy, group, sort, sortOn, Map, splitOn, getLines)
import Data.Graph.Inductive (Gr)
import Data.Type.Bool (Not)
import Data.Map qualified as M
import Data.Map (Map)


parseAttackType :: String -> AttackType
parseAttackType "fire" = Fire
parseAttackType "cold" = Cold
parseAttackType "slashing" = Slashing
parseAttackType "bludgeoning" = Bludgeoning
parseAttackType "radiation" = Radiation
parseAttackType s = error $ "parseAttackType: unknown attack type: " ++ s


parseWeaknesses :: String -> [AttackType]
parseWeaknesses s = case splitOn "weak to " s of
  [_] -> []
  [_, rest] -> parseAttackType <$> splitOn ", " (takeWhile (`notElem` ");") rest)


parseImmunities :: String -> [AttackType]
parseImmunities s = case splitOn "immune to " s of
  [_] -> []
  [_, rest] -> parseAttackType <$> (splitOn ", " $ takeWhile (`notElem` ";)") rest)


parseGroup :: UnitType -> String -> Group
parseGroup t s = Group { unitType = t
                     , units = read $ head ws
                     , hitpoints = read $ ws!!4
                     , attackDamage = read $ head $ drop (n-6) ws
                     , attackType = parseAttackType $ head $ drop (n-5) ws
                     , initiative = read $ last $ ws
                     , weaknesses = parseWeaknesses s
                     , immunities = parseImmunities s 
                     }
  where
    ws = words s
    n = length ws


data UnitType = Infection | Immune deriving (Eq, Ord, Show)
type Hitpoints = Int
type Damage = Int
data AttackType = Fire | Cold | Slashing | Bludgeoning | Radiation deriving (Show, Eq, Ord)
data Group = Group { unitType :: UnitType
                   , units :: Int
                   , hitpoints :: Hitpoints
                   , attackDamage :: Damage
                   , attackType :: AttackType
                   , initiative :: Int
                   , weaknesses :: [AttackType]
                   , immunities :: [AttackType] } deriving (Show, Eq)
type Key = Int
type Units = Map Key Group
type Battle = (Key, Key)


power :: Group -> Int
power g = units g * attackDamage g


-- target selection order
instance Ord Group where
  (<=) :: Group -> Group -> Bool
  a <= b = if power a == power b then initiative a >= initiative b else power a >= power b


greaterAttack :: Group -> Group -> Group -> Ordering
greaterAttack g a b
  | (damage g a > damage g b) = GT
  | (damage g a == damage g b) && (power a > power b) = GT
  | (damage g a == damage g b) && (power a == power b) && (initiative a > initiative b) = GT
  | otherwise = LT


opposition :: UnitType -> Units -> Units
opposition typ = M.filter (\t -> (unitType t) /= typ)


maximumByWithKey :: ((k, v) -> (k, v) -> Ordering) -> M.Map k v -> Maybe (k, v)
maximumByWithKey comp m
  | M.null m = Nothing
  | otherwise  = Just $ maximumBy comp (M.toList m)


chooseTarget :: Key -> Group -> Units -> Maybe Key
chooseTarget k gp bs
  | null canDamage = Nothing
  | otherwise =  fst <$> maximumByWithKey (\(_,x) (_,y) -> greaterAttack gp x y) canDamage
  where
    opp = opposition (unitType gp) bs
    canDamage :: Units
    canDamage = M.filter ((>0) . damage gp) opp


damage :: Group -> Group -> Int
damage a b
  | attackType a `elem` immunities b = 0
  | attackType a `elem` weaknesses b = 2 * power a
  | otherwise = power a


battles :: Units -> [Battle]
battles startUs = sortOn (negate . initiative . (startUs M.!) . fst) $ go startAs startUs
  where
    startAs = sortOn snd (M.assocs startUs) -- target selection order
    cmp = negate . initiative  -- battle order
    go :: [(Key, Group)] -> Units -> [Battle]
    go [] _ = []
    go ((k, gp):ks) us = case chooseTarget k gp us of
                     Nothing -> go ks us
                     Just l -> (k,l) : go ks (M.delete l us)


applyBattle :: Units -> Battle -> Units
applyBattle us (gix, hix)
  | hix `M.notMember` us = us
  | gix `M.notMember` us = us
  | kills >= units h = M.delete hix us
  | otherwise = M.insert hix (h {units = (units h - kills)}) us
  where
    h = us M.! hix
    g = us M.! gix
    dam = damage g h
    kills = dam `quot` hitpoints h


war :: Int -> Units -> Units
war n us
  | n == 0 = us
  | length (group $ unitType <$> M.elems us) == 1 = us
  | otherwise = war (n-1) $ foldl applyBattle us $ battles us


day24 :: IO ()
day24 = do
  inLines <- getLines 24
  let immune :: [Group]
      immune = sort $ parseGroup Immune <$> take 10 (tail inLines)
      infection :: [Group]
      infection = sort $ parseGroup Infection <$> take 10 (drop 13 inLines)
      boost = 46 -- 44 infection, 46 immunity, 45 stalemate

      us1, us2 :: Units
      us1 = M.fromList $ ((\g -> (initiative g, g)) <$> immune)  ++ ((\g -> (initiative g, g)) <$> infection)
      us2 = M.fromList $ ((\g -> (initiative g, g)) . (\g -> g {attackDamage = (attackDamage g) + boost}) <$> immune)  ++ ((\g -> (initiative g, g)) <$> infection)

      w1 = sum $ units <$> war 1000 us1
      w2 = sum $ units <$> war 8000 us2

  putStrLn $ "Day24: part1: " ++ show (w1) 
  putStrLn $ "Day24: part2: " ++ show (w2) 


  return ()


test = ["Immune System:"
  , "17 units each with 5390 hit points (weak to radiation, bludgeoning) with an attack that does 4507 fire damage at initiative 2"
  , "989 units each with 1274 hit points (immune to fire; weak to bludgeoning, slashing) with an attack that does 25 slashing damage at initiative 3"
  , ""
  , "Infection:"
  , "801 units each with 4706 hit points (weak to radiation) with an attack that does 116 bludgeoning damage at initiative 1"
  , "4485 units each with 2961 hit points (immune to radiation; weak to fire, cold) with an attack that does 12 slashing damage at initiative 4"
  ]

