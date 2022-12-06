import Data.List.Split
import Data.List

main :: IO ()
main = do
  ls <- readFile "6.txt"

  putStrLn "Part 1"
  print (process 4 ls)

  putStrLn "Part 2"
  print (process 14 ls)

process :: Int -> String -> Int
process m = extract . generate
  where
    extract :: [String] -> Int
    extract xs =
      case head (filter needle haystack) of
        (x,_) -> x + (m - 1)
      where
        haystack = zip [1..] xs

    generate :: String -> [String]
    generate [] = []
    generate xs
      | length (take m xs) < m = []
      | otherwise = take m xs : generate (tail xs)

    needle :: (Int,String) -> Bool
    needle (_, xs) = length (nub xs) == length xs

