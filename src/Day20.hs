
module Day20 where


import Utils hiding (Tree, TreeF)
import Control.Applicative
import qualified Data.Set as S
import Data.Either (fromRight)
import Data.Map qualified as M
import qualified Text.Parsec as P
import qualified Text.Parsec.String as PS


data D = N | S | E | W deriving (Show, Eq, Ord)

data Tok = St | En | Rt | Lt | Or | Dr D deriving (Show, Eq, Ord)

parseToks :: String -> [Tok]
parseToks [] = []
parseToks (c:cs)
  | '^' == c = St : parseToks cs
  | '$' == c = En : parseToks cs
  | '(' == c = Lt : parseToks cs
  | ')' == c = Rt : parseToks cs
  | '|' == c = Or : parseToks cs
  | 'N' == c = Dr N : parseToks cs
  | 'S' == c = Dr S : parseToks cs
  | 'E' == c = Dr E : parseToks cs
  | 'W' == c = Dr W : parseToks cs
  | otherwise = error $ "parseToks: unexpected char: " ++ [c]

type Edge = (Coord, Coord)

mkEdge :: Coord -> Coord -> Edge
mkEdge x y
  | x <= y    = (x, y)
  | otherwise = (y, x)

type Parser = P.Parsec [Tok] Coord


tok :: Tok -> Parser ()
tok t = P.try $ guard . (== t) =<< P.anyToken

anySteps :: Parser (Set Edge)
anySteps = fmap S.unions . P.many $ P.try normalStep P.<|> branchStep

normalStep :: Parser (Set Edge)
normalStep = do
    currPos <- P.getState
    Dr d <- P.anyToken -- will fail when not a direction
    let newPos = currPos + case d of
          N -> up
          E -> rt
          S -> dn
          W -> lt
    P.setState newPos
    S.insert (mkEdge currPos newPos) <$> anySteps


branchStep :: Parser (Set Edge)
branchStep = (tok Lt `P.between` tok Rt) $ do
    initPos <- P.getState
    fmap S.unions . (`P.sepBy` tok Or) $ do
      P.setState initPos
      anySteps


buildEdges :: Parser (Set Edge)
buildEdges = (tok St `P.between` tok En) anySteps


neighbours :: Coord -> [Coord]
neighbours p = (p +) <$> directions4


roomDistances :: Set Edge -> [Int]
roomDistances edges = go 0 S.empty (0, 0)
  where
    go :: Int -> Set Coord -> Coord -> [Int]
    go n visited room = (n :) $ concatMap (go (n + 1) (S.insert room visited)) ns
      where
        ns = filter ((`S.member` edges) . mkEdge room) -- is a valid edge
           . filter (`S.notMember` visited) -- not already visited
           $ neighbours room


day20 :: IO ()
day20 = do
  inWords <- getLines 20
  let toks = parseToks $ inWords !! 0
      edges = fromRight S.empty $ P.runParser buildEdges (0, 0) "" toks

  --putStrLn $ "Day20: part1: " ++ show (toks)
  --putStrLn $ "Day20: part1: " ++ show (edges)
  putStrLn $ "Day20: part1: " ++ show (maximum (roomDistances edges))
  putStrLn $ "Day20: part2: " ++ show (length . filter (>= 1000) $ roomDistances edges)

  return ()

test1, test2, test3, test4 :: String
test1 = "^ENWWW(NEEE|SSE(EE|N))$" -- 10
test2 = "^ENNWSWW(NEWS|)SSSEEN(WNSE|)EE(SWEN|)NNN$" -- 18
test3 = "^ESSWWN(E|NNENN(EESS(WNSE|)SSS|WWWSSSSE(SW|NNNE)))$" -- 23
test4 = "^WSSEESWWWNW(S|NENNEEEENN(ESSSSW(NWSW|SSEN)|WSWWN(E|WWS(E|SS))))$" -- 31

