{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}
module Main where

import           Control.Monad
import           Control.Monad.State
import           Control.Monad.Writer
import           Data.Either
import           Data.Function
import           Data.List
import           Data.List.Split
import           Data.Map.Strict      (Map)
import qualified Data.Map.Strict      as M
import           Data.Monoid
import           Data.Tree
import           Debug.Trace          (traceShow)

main :: IO ()
main = do
  cmds <- fmap parseCommand . lines <$> readFile "7.txt"
  let disk = mkDisk cmds

  putStrLn "Part 1"
  print (countDirectorySizes disk)

  putStrLn "Part 2"

  let
    totalUsed           = countTotalSize (mkDisk cmds)
    totalDiskSpace      = 70000000
    requiredUpdateSpace = 30000000
    unused              = totalDiskSpace - totalUsed
    needToFree          = requiredUpdateSpace - unused

  print (findMinimalFreeSpace disk needToFree)

findMinimalFreeSpace :: Disk Int -> Int -> (String, Int)
findMinimalFreeSpace disk needToFree = head
  [ (dir, size)
  | (dir, size) <- sortBy (compare `on` snd) (accumSizes disk)
  , size > needToFree
  ]

data Cmd
  = Cd String
  | Ls
  | Dir String
  | File Int
  | Unknown String

data Disk a
  = FileSize a
  | Directory String [Disk a]

directorySize :: Disk Int -> Int
directorySize (FileSize n) = n
directorySize (Directory _ children) =
  sum (directorySize <$> children)

countDirectorySizes :: Disk Int -> Int
countDirectorySizes = go 0
  where
    go acc (FileSize _) = 0
    go acc d@(Directory name children)
      | directorySize d <= 100000 =
          directorySize d + sum (countDirectorySizes <$> children)
      | otherwise =
          sum (countDirectorySizes <$> children)

accumSizes :: Disk Int -> [(String,Int)]
accumSizes d = execState (go d) []
  where
    push k v = modify ((k,v):)
    go (FileSize _) = pure ()
    go d@(Directory name children) = do
      push name (directorySize d)
      forM_ children go

countTotalSize :: Disk Int -> Int
countTotalSize = go 0
  where
    go acc (FileSize _) = 0
    go acc d@(Directory name children) = directorySize d

insertDisk
  :: [String]
  -- ^ Stack
  -> Disk Int
  -- ^ file or directory
  -> Disk Int
  -- ^ disk
  -> Disk Int
  -- ^ updated disk
insertDisk [k] new dir@(Directory d children)
  | k == d = Directory d (new : children)
  | otherwise = dir
insertDisk (k:ks) new dir@(Directory d children)
  | k == d = Directory d (insertDisk ks new <$> children)
  | otherwise = dir
insertDisk _ _ d = d

emptyDisk :: Disk a
emptyDisk = Directory "/" []

mkDisk :: [Cmd] -> Disk Int
mkDisk cmds = fst $ flip execState (emptyDisk, ["/"]) $ do
  forM_ (tail cmds) $ \case
    Cd ".." -> do
      (disk, stack) <- get
      put (disk, tail stack)
    Cd dir -> do
      (disk, stack) <- get
      put (disk, dir:stack)
    Ls ->
      pure ()
    Dir dir -> do
      (disk, stack) <- get
      let newDisk = insertDisk (reverse stack) (Directory dir []) disk
      put (newDisk, stack)
    File size -> do
      (disk, stack) <- get
      let newDisk = insertDisk (reverse stack) (FileSize size) disk
      put (newDisk, stack)
    Unknown e ->
      error e

parseCommand :: String -> Cmd
parseCommand (words -> ["$", "cd", d])       = Cd d
parseCommand (words -> ["$","ls"])           = Ls
parseCommand (words -> ["dir", dir])         = Dir dir
parseCommand (words -> [read -> size, file]) = File size
parseCommand xs                              = Unknown xs
