import Data.List.Split
import Data.List

main :: IO ()
main = do
  ls <- readFile "1.txt"
  let xs =
         [ sum (read <$> xs)
         | xs <- splitOn [""] (lines ls)
         ]
  putStrLn "Part 1"
  print $ maximum xs
  putStrLn "Part 2"
  print $ sum $ take 3 $ reverse (sort xs)

