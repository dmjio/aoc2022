{-# LANGUAGE ViewPatterns #-}
module Main where

import qualified Data.IntMap.Strict  as I ( (!), assocs, elems, alter, fromList, insert, insertWith, IntMap )
import           Data.List           ( foldl', transpose )
import           Data.List.Split     ( splitOn )
import           Data.Maybe          ( listToMaybe, mapMaybe )

type Move = (Int, (Int,Int))
type Stacks = I.IntMap String

main :: IO ()
main = do
  [init -> top, bottom] <- splitOn [""] . lines <$> readFile "5.txt"
  let
    stacks = I.fromList $ zip [1..] $
      [ concat (words l)
      | l <- transpose (concatMap pad <$> top)
      , not $ null (words l)
      ]

    moves :: [Move]
    moves = parseMove <$> bottom

    pad :: Char -> String
    pad x | x `elem` "[] " = " "
          | True = pure x

  putStrLn "Part 1"
  putStrLn $ getResult (moveStackPart1 stacks moves)

  putStrLn "Part 2"
  putStrLn $ getResult (moveStackPart2 stacks moves)

parseMove :: String -> (Int, (Int,Int))
parseMove xs =
  case words xs of
    ["move", num, "from", x, "to", y] ->
      (read num, (read x, read y))

pushStack :: Int -> Stacks -> Char -> Stacks
pushStack k stacks v = I.insertWith (++) k [v] stacks

moveStackPart1 :: Stacks -> [Move] -> Stacks
moveStackPart1 = foldl' shift
  where
    shift :: Stacks -> Move -> Stacks
    shift stacks (count, (from,to)) =
      I.insert from old (foldl' (pushStack to) stacks new)
        where
          (new, old) = splitAt count (stacks I.! from)

moveStackPart2 :: Stacks -> [Move] -> Stacks
moveStackPart2 = foldl' shift
  where
    shift :: Stacks -> Move -> Stacks
    shift stacks (count, (from,to)) | count > 1 =
      I.insert from old (I.alter (fmap (new ++)) to stacks)
        where
          (new, old) = splitAt count (stacks I.! from)

    shift stacks (count, (from,to)) =
      I.insert from old (foldl' (pushStack to) stacks new)
        where
          (new, old) = splitAt count (stacks I.! from)

showStacks :: Stacks -> IO ()
showStacks = mapM_ print . I.assocs

getResult :: Stacks -> String
getResult = mapMaybe listToMaybe . I.elems

