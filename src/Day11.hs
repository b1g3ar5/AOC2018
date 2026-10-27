{-# LANGUAGE BangPatterns, TupleSections #-}

module Day11 where


import Utils hiding (repeat, insert, fix)
import Control.Comonad
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import Data.Bifunctor
import Data.Function
import System.TimeIt ( timeIt )


serialNumber :: Int
serialNumber = 7139

gridSize :: Int
gridSize = 300 

coords :: [[Coord]]
coords = (\y -> (,y) <$> [0..(gridSize-1)]) <$> [0..(gridSize-1)] 


data BoxedImage a = BoxedImage
    { biWidth  :: !Int
    , biHeight :: !Int
    , biData   :: !(Vector a)
    }


instance Functor BoxedImage where
    fmap f (BoxedImage w h d) = BoxedImage w h (fmap f d)


data FocusedImage a = FocusedImage
    { fiBoxedImage :: !(BoxedImage a)
    , fiX          :: !Int
    , fiY          :: !Int
    }


ps :: Vector Int
ps = V.fromList $ concat $ (powerLevel <$>) <$> coords


bi :: BoxedImage Int
bi = BoxedImage gridSize gridSize ps

fi :: FocusedImage Int
fi = focus bi


getSum :: Int -> Int
getSum ix
  | ix<0 = 0
  | ix >= gridSize*gridSize = error "index too big in getSumF"
  | ix == 0 = ps V.! 0
  | ix < gridSize = ps V.! ix + getSum (ix-1)
  | ix `mod` gridSize == 0 = ps V.! ix + getSum (ix-gridSize)
  | otherwise = ps V.! ix + getSum (ix-1) + getSum (ix-gridSize) - getSum (ix-gridSize-1)


-- Parameterise the recursion so that memoise will work
getSumF :: (Int -> Int) -> Int -> Int
getSumF f ix
  | ix<0 = 0
  | ix >= gridSize*gridSize = error "index too big in getSumF"
  | ix == 0 = ps V.! 0
  | ix < gridSize = ps V.! ix + f (ix-1)
  | ix `mod` gridSize == 0 = ps V.! ix + f (ix-gridSize)
  | otherwise = ps V.! ix + f (ix-1) + f (ix-gridSize) - f (ix-gridSize-1)


memoedSum :: Int -> Int
memoedSum = (sumTable U.!)


sumTable :: U.Vector Int
sumTable = U.generate (gridSize*gridSize) (simpleMemoise getSumF)


powerLevel :: Coord -> Int
powerLevel (x,y) = (((rid * (y+1) + serialNumber) * rid) `div` 100) `mod` 10 - 5
  where
    rid = (x+1)+10


focus :: BoxedImage a -> FocusedImage a
focus bi
    | biWidth bi > 0 && biHeight bi > 0 = FocusedImage bi 0 0
    | otherwise = error "Cannot focus on empty images"


unfocus :: FocusedImage a -> BoxedImage a
unfocus (FocusedImage bi _ _) = bi


instance Functor FocusedImage where
    fmap f (FocusedImage bi x y) = FocusedImage (fmap f bi) x y


instance Comonad FocusedImage where
    extract (FocusedImage bi x y) = biData bi V.! (y * biWidth bi + x)

    extend f (FocusedImage bi@(BoxedImage w h _) x y) = FocusedImage
      (BoxedImage w h $ V.generate (w * h) $ \i ->
          let (y', x') = i `divMod` w
          in f (FocusedImage bi x' y')) x y


neighbour :: Int -> Int -> FocusedImage a -> Maybe (FocusedImage a)
neighbour dx dy (FocusedImage bi x y)
    | outOfBounds = Nothing
    | otherwise   = Just (FocusedImage bi x' y')
  where
    x'          = x + dx
    y'          = y + dy
    outOfBounds = x' < 0 || x' >= biWidth bi || y' < 0 || y' >= biHeight bi


boxTotal1 :: Int -> FocusedImage Int -> Int
boxTotal1 n pixel = sum [ extract p | x <- [-n, (1-n)..n], y <- [-n, (1-n)..n] , p <- maybeToList (neighbour x y pixel)]


getMax :: Int -> ((Int, Coord), Int)
getMax n = (second (`quotRem` gridSize) $ maxIndex (V.toList $ biData $ unfocus filtered), n)
  where
    filtered = extend (boxTotal1 n) fi


-- Returns the maximum and the index of the maximum
maxIndex :: Ord a => [a] -> (a, Int)
maxIndex xs = (mx, head $ filter ((== mx) . (xs !!)) [0..])
  where
    mx = maximum xs


boxTotal :: Coord -> Int -> Int -> Int
boxTotal (0,0) width height = memoedSum $ width-1 + ((height-1)*gridSize)
boxTotal (0,y) width height = memoedSum (width-1 + ((y+height-1)*gridSize))
                            - memoedSum (width-1 + ((y-1)*gridSize))
boxTotal (x,0) width height = memoedSum (x+width-1)
                            - memoedSum (x-1)
boxTotal (x,y) width height = br - bl - tr + tl
  where
    tl = memoedSum $ (x-1) + ((y-1)*gridSize)
    br = memoedSum $ x+width-1 + ((y+height-1)*gridSize)
    tr = memoedSum $ x+width-1 + ((y-1)*gridSize)
    bl = memoedSum $ (x-1) + ((y+height-1)*gridSize)


-- Returns the Coord of the top left, the box total and the size of the box
getBestMax :: (Coord, (Int, Int))
getBestMax = maximumBy (comparing $ fst . snd) sums
  where
    sums = [ ((x, y), (boxTotal (x, y) sz sz, sz)) | sz <- [3..299], x <- [0..(gridSize-sz-1)], y <- [0..(gridSize-sz-1)] ]




day11 :: IO ()
day11 = do
  putStrLn $ "Day11: part1: " ++ show (getMax 1)
  putStrLn $ "Day11: part2: " ++ show (bimap (bimap (+ 1) (+ 1)) snd getBestMax)


  return ()


simpleMemoise :: ((Int -> Int) -> Int -> Int) -> (Int -> Int)
simpleMemoise funcToMemoise = fastestFunc
  where
    tree = funcToMemoise fastestFunc <$> nats'
    fastestFunc = index' tree


nats' :: Tree Int
nats' = go 0 1
  where
    go !n !s = Tree (go l s') n (go r s')
        where
          l = n + s
          r = l + s
          s' = s * 2


index' :: Tree a -> Int -> a
index' (Tree _ m _) 0 = m
index' (Tree l _ r) n = case (n - 1) `divMod` 2 of
    (q,0) -> index' l q
    (q,1) -> index' r q
    _ -> error "This is not possible in index'"


data Tree a = Tree (Tree a) a (Tree a)


instance Functor Tree where
    fmap f (Tree l m r) = Tree (fmap f l) (f m) (fmap f r)

