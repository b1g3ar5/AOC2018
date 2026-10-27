{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}

module Day5 where


import Utils (getRaw)
import Data.Char (toUpper)


match :: Char -> Char -> Bool
match x y = x /= y && toUpper y == toUpper x


simplify1 :: String -> Int
simplify1  = length . go ""
  where
    go acc [] = acc
    go [] (x:xs) = go [x] xs
    go (a:as) [x] = if match a x then as else x:a:as
    go (a:as) (x:xs)
      | match a x = go as xs
      | otherwise = go (x:a:as) xs


simplify2 :: String -> Int 
simplify2 xs = minimum $ go <$> ['a'..'z']
  where
    go :: Char -> Int
    go c = simplify1 $ filter (\x -> x /= c && x /= toUpper c) xs


day5 :: IO ()
day5 = do
  input <- getRaw 5
  putStrLn $ "Day5: part1: " ++ show (simplify1 input)
  putStrLn $ "Day5: part2: " ++ show (simplify2 input)

  return ()


