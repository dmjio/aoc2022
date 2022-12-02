module Main where

sample
  = "A Y\n\
    \B X\n\
    \C Z"

data Move
  = Rock
  | Paper
  | Scissors
  deriving (Eq, Show, Enum)

win :: Move -> Move
win Scissors = Rock
win x = succ x

lose :: Move -> Move
lose Rock = Scissors
lose x = pred x

data Status
  = Won
  | Tied
  | Lost
  deriving (Show)

scoreStatus :: Status -> Int
scoreStatus Won  = 6
scoreStatus Tied = 3
scoreStatus Lost = 0

scoreMove :: Move -> Int
scoreMove = (+1) . fromEnum

determineWinner :: Pair -> Status
determineWinner (x, y)
  | x == y     = Tied
  | win x == y = Won
  | otherwise  = Lost

parseMove :: Char -> Move
parseMove 'A' = Rock
parseMove 'B' = Paper
parseMove 'C' = Scissors
parseMove 'X' = Rock
parseMove 'Y' = Paper
parseMove 'Z' = Scissors

predict :: Move -> Move -> Move
predict x Rock     = lose x
predict x Paper    = x
predict x Scissors = win x

type Pair = (Move, Move)

scorePairPart1 :: Pair -> Int
scorePairPart1 pair@(_, x)
  = scoreStatus (determineWinner pair)
  + scoreMove x

scorePairPart2 :: Pair -> Int
scorePairPart2 pair
  = scoreStatus (determineWinner newPair)
  + scoreMove prediction
  where
    newPair = (fst pair, prediction)
    prediction = uncurry predict pair

parsePair :: [Char] -> (Move, Move)
parsePair [x,' ', y] = (parseMove x, parseMove y)

main :: IO ()
main = do
  -- sample
  xs <- map parsePair . lines <$> pure sample
  score xs

  -- acutal
  xs <- map parsePair . lines <$> readFile "2.txt"
  score xs

score :: [Pair] -> IO ()
score xs = do
  putStrLn "Part 1"
  print $ sum (scorePairPart1 <$> xs)
  putStrLn "Part 2"
  print $ sum (scorePairPart2 <$> xs)
