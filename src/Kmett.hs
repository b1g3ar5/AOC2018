{-# LANGUAGE LambdaCase #-}

module Kmett where

import Data.Functor.Base
import Data.Functor.Foldable
import Data.Tree
import Data.List
import Control.Monad.Reader
import Control.Comonad
import Control.Comonad.Cofree

myTree :: Tree Int
myTree = Node 0 [Node 1 [], Node 2 [], Node 3 [Node 31 [Node 311 [Node 3111 [], Node 3112 []]]]]

kmettMain :: IO ()
kmettMain = do
  --putStrLn $ drawTree $ show <$> myTree
  putStrLn $ pprint1 myTree
  putStrLn $ pprint2 myTree
  putStrLn $ pprint3 myTree
  putStrLn $ pprint4 myTree
  putStrLn $ pprint4 $ insertLeftmost 999 myTree
  putStrLn $ pprint4 $ insertRightmost 888 myTree
  putStrLn $ pprint5 myTree




pprint1 :: Tree Int -> String
pprint1 = cata $ \case
  NodeF i [] -> show i
  NodeF i ss -> show i ++ ": [" ++ intercalate ", " ss ++ "]"  


pprint2 :: Tree Int -> String
pprint2 = flip runReader 0 . cataA go
  where
    go :: TreeF Int (Reader Int String)
        -> Reader Int String
    go (NodeF i rss) = do
      -- rss :: [Reader Int String]
      -- ss  :: [String]
      ss <- local (+ 2) $ sequence rss
      indent <- ask
      let s = replicate indent ' ' ++ "* " ++ show i
      pure $ intercalate "\n" (s : ss)


-- Carrier type is now a function
-- So we need a list of functions, one for each subnode
-- 2 is added to the indent for the call to the subnode functions...
pprint3 :: Tree Int -> String
pprint3 t = cataA go t 0
  where
    go :: TreeF Int (Int -> String)
        -> Int -> String
    go (NodeF i fs) indent
        -- fs :: [Int -> String]
      = let indent' = indent + 2
            ss = map (\f -> f indent') fs
            s = replicate indent ' ' ++ "* " ++ show i
        in intercalate "\n" (s : ss)      


-- para - each node has the carrier (Reader Int String in this case) and also a Tree
pprint4 :: Tree Int -> String
pprint4 = flip runReader 0 . para go
  where
    go :: TreeF Int (Tree Int, Reader Int String)
        -> Reader Int String
    go (NodeF i trss) = do
      -- trss :: [(Tree Int, Reader Int String)]
      -- ts   :: [Tree Int] - one for each node
      -- rss  :: [Reader Int String] - one for each node
      -- ss   :: [String]
      let (ts, rss) = unzip trss
      let count = sum $ fmap length ts
      ss <- local (+ 2) $ sequence rss
      indent <- ask
      let s = replicate indent ' '
            ++ "* " ++ show i
            ++ " (" ++ show count ++ ")" -- annotate the node with the node count
      pure $ intercalate "\n" (s : ss)        



insertLeftmost :: Int -> Tree Int -> Tree Int
insertLeftmost new = para go
  where
    -- The algebra output is a Tree, also the extra para tree is there
    -- So each node has a couple of trees
    go :: TreeF Int (Tree Int, Tree Int) -> Tree Int
    go (NodeF i []) = Node i [Node new []]
    go (NodeF i ((_orig, recur) : tts))
        -- tts :: [(Tree Int, Tree Int)]
      = let (origs, _recurs) = unzip tts
        -- Each node has the original excep the leftest which uses the recur
        in Node i (recur : origs)


insertRightmost :: Int -> Tree Int -> Tree Int
insertRightmost new = para go
  where
    go :: TreeF Int (Tree Int, Tree Int) -> Tree Int
    go (NodeF i []) = Node i [Node new []]
    go (NodeF i tts) = Node i (origs ++ [recur])
      where
        (origs, _recurs) = unzip $ init tts
        (_orig, recur) = last tts



rollup :: [Cofree (TreeF node) label] -> ([node], [label])
rollup [_ :< NodeF node cofrees] = (node : nodes, label)
  where
    (nodes, label) = rollup cofrees
rollup cofrees = ([], fmap extract cofrees)


pprint5 :: Tree Int -> String
pprint5 t = histo go t 0
  where
    go :: TreeF Int (Cofree (TreeF Int) (Int -> String))
        -> Int -> String
    go (NodeF node cofrees) indent
        -- cofrees :: [Cofree (TreeF Int) (Int -> String)]
        -- fs :: [Int -> String]
      = let indent' = indent + 2
            (nodes, fs) = rollup cofrees
            ss = map (\f -> f indent') fs
            s = replicate indent ' '
              ++ "* " ++ intercalate " / " (fmap show (node : nodes))
        in intercalate "\n" (s : ss)

