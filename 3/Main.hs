import Data.List       ( elemIndices, intersect, nub )
import Data.List.Split ( chunksOf )
import Data.Maybe      ( fromMaybe )
import Data.Monoid     ( Sum(Sum) )
import Data.Function

main :: IO ()
main = do
  ls <- lines <$> readFile "3.txt"

  -- Part 1
  print $ foldMap (priority . shared . splitLine) ls

  -- Part 2
  print $ flip foldMap (chunksOf 3 ls) $ \chunk ->
    priority (foldl1 (intersect `on` nub) chunk)

priority :: String -> Sum Int
priority xs = foldMap (Sum . succ) (head xs `elemIndices` chars)
  where
    chars = ['a'..'z'] ++ ['A'..'Z']

splitLine :: String -> (String, String)
splitLine xs = splitAt (length xs `div` 2) xs

shared :: (String, String) -> String
shared = uncurry (intersect `on` nub)
