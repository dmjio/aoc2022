module Main where

import Data.List       ( intersect, sort, union )
import Data.List.Split ( splitOn )
import Data.Monoid

main :: IO ()
main = do
  ls <- fmap parse . lines <$> readFile "4.txt"
  -- ls <- fmap parse .lines <$> readFile "sample.txt"
  putStrLn "Part 1"
  print (foldMap (uncurry part1) ls)
  putStrLn "Part 2"
  print (foldMap (uncurry part2) ls)

part1 :: [Int] -> [Int] -> Sum Int
part1 xs ys
  | let zs = sort (xs `union` ys)
  , any (== zs) [xs, ys] = 1
  | otherwise = 0

part2 :: [Int] -> [Int] -> Sum Int
part2 xs ys
  | null (xs `intersect` ys) = 0
  | otherwise = 1

parse :: String -> ([Int],[Int])
parse xs = do
  let
    [l,r] = splitOn "," xs
    ([l1,l2],[r1,r2]) = (splitOn "-" l, splitOn "-" r)
  ( [read l1 .. read l2], [read r1 .. read r2] )
