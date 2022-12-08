import           Control.Arrow ( Arrow(first) )
import           Control.Monad ( forM_ )
import           Data.Bits     ( Bits(popCount) )
import           Data.Char     ( digitToInt )
import           Data.Map      ( Map )
import qualified Data.Map      as M ( (!), fromList, keys, lookup )
import           Data.Maybe    ( catMaybes, isJust )
import           Data.Tuple    ( swap )

main :: IO ()
main = do
  str <- readFile "8.txt"
  let
    ls = lines str
    x = length ls
    y = length (head ls)
    grid = readGrid (concat ls) (x,y)

  putStrLn "Part 1"
  print $ sum $ fmap popCount $
    [ visible coord grid
    | coord <- M.keys grid
    ]

  putStrLn "Part 2"
  print $ maximum
    [ scenic coord grid
    | coord <- M.keys grid
    ]

readGrid :: String -> (Int,Int) -> Map (Int,Int) Int
readGrid str (x,y) =
  let
    coords = (,) <$> [0 .. x - 1] <*> [0 .. y - 1]
  in
    M.fromList (first swap <$> zip coords (digitToInt <$> str))

displayGrid :: (Int,Int) -> Map (Int,Int) Int -> IO ()
displayGrid (x,y) grid =
  forM_ [0 .. y - 1] $ \y' -> do
    putChar '\n'
    forM_ [0 .. x - 1] $ \x' ->
       putStr $ show (grid M.! (y',x'))

helper :: (Int,Int) -> Map (Int, Int) Int -> ([[Int]] -> Int -> a) -> a
helper coord@(x,y) grid f =
  let
    top    = fetch $ zip (repeat x) [ y + 1 .. ]
    right  = fetch $ zip [ x + 1 .. ] (repeat y)
    bottom = fetch $ zip (repeat x) [ y - 1, y - 2 .. ]
    left   = fetch $ zip [ x - 1, x - 2 .. ] (repeat y)

    value  = grid M.! coord

    fetch :: [(Int,Int)] -> [Int]
    fetch  = catMaybes . takeWhile isJust . fmap (`M.lookup` grid)
  in f [top, right, bottom, left] value

visible :: (Int, Int) -> Map (Int, Int) Int -> Bool
visible coord grid = helper coord grid $ \xxs value ->
  or
  [ all (< value) xs
  | xs <- xxs
  ]

scenic :: (Int, Int) -> Map (Int, Int) Int -> Int
scenic coord grid = helper coord grid $ \xxs value ->
  product
     [ length (takeUntil value xs)
     | xs <- xxs
     , not (null xs)
     ]

takeUntil :: Ord a => a -> [a] -> [a]
takeUntil x xs = do
  let (ls, rs) = (takeWhile (<x) xs, dropWhile (<x) xs)
  ls ++ [ head (takeWhile (>=x) rs) | not (null rs) ]
