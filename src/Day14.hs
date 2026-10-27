{-# LANGUAGE BangPatterns #-}

module Day14 where


import Utils
import Data.Sequence (Seq(..), fromList, index)
import Control.Monad.ST (ST, runST)
import Data.Word (Word8)
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VM
import System.TimeIt


------------------------------------------------------------------------------
-- Old version: lazy list of recipes, built on top of a Seq.
--
-- Slow for part 2 because:
--   * recipesOld is a top level CAF, so all ~20M list cells stay alive
--   * each step does two O(log n) Seq lookups and allocates finger tree nodes
--   * toDigits goes via show/read, and read is very slow
------------------------------------------------------------------------------

toDigits :: Int -> [Int]
toDigits x = (\c -> read [c]) <$> show x


recipesOld :: [Int]
recipesOld = 3 : 7 : go (0, 1, fromList [3,7])
  where
    go :: (Int, Int, Seq Int) -> [Int]
    go (f1, f2, rs) = total ++ go (next1, next2, newRs)
      where
        r1, r2, next1, next2 :: Int
        !r1 = rs `index` f1
        !r2 = rs `index` f2
        next1 = (f1 + r1 + 1) `mod` length newRs
        next2 = (f2 + r2 + 1) `mod` length newRs
        total :: [Int]
        total = toDigits $ r1 + r2
        newRs :: Seq Int
        !newRs = foldl (|>) rs total

------------------------------------------------------------------------------
-- New version: a mutable unboxed vector in ST.
--
--   * the board is a flat array of bytes: O(1) reads and writes, no boxing,
--     and ~20MB for 20M recipes instead of gigabytes of heap
--   * the vector doubles in size when full, so appends are amortised O(1)
--   * scores are at most 9 + 9 = 18, so the digits are just 1 and s - 10
--   * the stop test runs after every single append, so part 2 never builds
--     a list or calls tails
------------------------------------------------------------------------------

-- | Run the elves until @done@ says stop. @done@ is called after every
-- recipe is appended, with the board and its new length.
-- Returns the frozen board.
makeBoard :: (forall s. VM.MVector s Word8 -> Int -> ST s Bool) -> VU.Vector Word8
makeBoard done = runST $ do
  v0 <- VM.new 1024
  VM.write v0 0 3
  VM.write v0 1 7
  let -- Append one score, growing the vector if it's full.
      push v n x = do
        v' <- if n < VM.length v then pure v else VM.grow v (VM.length v)
        VM.write v' n x
        stop <- done v' (n + 1)
        pure (v', n + 1, stop)

      loop !v !n !e1 !e2 = do
        r1 <- VM.read v e1
        r2 <- VM.read v e2
        let s = r1 + r2
        (v', n', stop) <-
          if s >= 10
            then do
              (va, na, sa) <- push v n 1
              if sa then pure (va, na, True) else push va na (s - 10)
            else push v n s
        if stop
          then pure (v', n')
          else loop v' n' ((e1 + 1 + fromIntegral r1) `mod` n')
                          ((e2 + 1 + fromIntegral r2) `mod` n')

  (v, n) <- loop v0 2 0 1
  VU.freeze (VM.slice 0 n v)


-- | Does the board (of length n) end with the target digits?
endsWith :: VU.Vector Word8 -> VM.MVector s Word8 -> Int -> ST s Bool
endsWith target v n
  | n < k = pure False
  | otherwise = go 0
  where
    k = VU.length target
    go i
      | i == k = pure True
      | otherwise = do
          x <- VM.read v (n - k + i)
          if x == target VU.! i then go (i + 1) else pure False


day14 :: IO ()
day14 = do
  inLines <- getLines 14
  let input :: Int
      input = read $ head inLines
      -- Don't drop the initial zero!
      target :: VU.Vector Word8
      target = VU.fromList $ (\c -> fromIntegral (ord c - ord '0')) <$> head inLines
      board1 = makeBoard (\_ n -> pure (n >= input + 10))
      board2 = makeBoard (endsWith target) 

  putStrLn $ "Day14: part1: " ++ concatMap show (VU.toList $ VU.slice input 10 board1)
  putStrLn $ "Day14: part2: " ++ show (VU.length board2 - VU.length target)

  return ()
