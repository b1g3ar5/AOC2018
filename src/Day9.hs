
module Day9 where


import Utils hiding (insert)


data Tape a = Tape { lt:: ![a], focus :: !a, rt :: ![a]} deriving (Show)


mkTape :: [a] -> Tape a
mkTape [] = error "Can't have a tape with an empty list"
mkTape (x:xs) = Tape [] x xs


maxTape :: Tape Int -> Int
maxTape (Tape ls x rs) = maximum $ x:ls ++ rs


add :: Int -> Tape Int -> Tape Int
add n (Tape ls x rs) = Tape ls (x+n) rs


-- insert after the focus
insert :: a -> Tape a -> Tape a
insert x (Tape ls f rs) = Tape (f:ls) x rs


remove :: Tape a -> (a, Tape a)
remove (Tape [] x []) = error "Tape has only one intem in remove"
remove (Tape ls x []) = (x, Tape (take n ls) r rs)
  where
    n = length ls `div` 2
    (r:rs) = reverse $ drop n ls
remove (Tape ls x (r:rs)) = (x, Tape ls r rs)


moveR :: Tape a -> Tape a
moveR (Tape [] f []) = Tape [] f []
moveR (Tape xs f []) = Tape (f : take n xs) y ys
  where
    n = length xs `div` 2
    (y:ys) = reverse $ drop n xs
moveR (Tape xs f (y:ys)) = Tape (f:xs) y ys


moveL :: Tape a -> Tape a
moveL (Tape [] f []) = Tape []f []
moveL (Tape [] f ys) = Tape xs x $ f : take n ys
  where
    n = length ys `div` 2
    (x:xs) = reverse $ drop n ys
moveL (Tape (x:xs) f ys) = Tape xs x (f:ys)


move :: Int -> Tape a -> Tape a
move n t@(Tape lt f rt)= 
  if n >=0 
    then 
      iterate moveR t !! n
    else
      iterate moveL t !! (-n)


type Players = Tape Int


go :: (Int, Players, Tape Int) -> (Int, Players, Tape Int)
go (n, ps, tp)
  | n `mod` 23 == 0 = (n+1, moveR $ add (n+rem) ps, t)
  | otherwise = (n+1, moveR ps, insert n $ moveR tp)
    where
      (rem, t)= remove $ move (-7) tp


run :: Int -> (Int, Players, Tape Int) -> (Int, Players, Tape Int)
run 0 s = s
run k (!n, !ps, !tp) = run (k - 1) (go (n, ps, tp))


{-
run :: Int -> (Int, Players, Tape Int) -> (Int, Players, Tape Int)
run 0 s = s
run k (!n, !ps, !tp) = run (k - 1) (go (n, ps, tp))
-}

day9 :: IO ()
day9 = do
  inWords <- getWords 9
  let nplayers = 405
      nmarbles = 70953
      (_, ps1, _) = run nmarbles (1, mkTape (replicate nplayers 0), Tape [] 0 [] )
      (_, ps2, _) = run (nmarbles*100) (1, mkTape (replicate nplayers 0), Tape [] 0 [] )

  putStrLn $ "Day9: part1: " ++ show (maxTape ps1)
  putStrLn $ "Day9: part2: " ++ show (maxTape ps2)

  return ()


