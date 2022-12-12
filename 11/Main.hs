{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
module Main where

import           Control.Lens        hiding (op)
import           Control.Monad.State
import           Data.Char
import           Data.IntMap.Strict  (IntMap)
import qualified Data.IntMap.Strict  as I
import           Data.List
import           Data.List.Split
import           Debug.Trace         (traceShow)
import           Prelude             hiding (round)
import           Text.Read           (readMaybe)

data Op = Add | Mul | Old
  deriving (Show, Eq)

data Monkey
  = Monkey
  { _monkey :: Integer
  , _items :: [Integer]
  , _op :: (Op, Val)
  , _divisible :: Integer
  , _ifTrue :: Int
  , _ifFalse :: Int
  } deriving (Show, Eq)

data Val = ValInt Integer | ValOld
  deriving (Show, Eq)

data Monkeys
  = Monkeys
  { _monkeys :: IntMap Monkey
  , _tosses :: IntMap Integer
  , _round :: Integer
  } deriving (Eq)

$(makeLenses ''Monkeys)
$(makeLenses ''Monkey)

instance Show Monkeys where
  show = showMonkeys

showMonkeys :: Monkeys -> String
showMonkeys (Monkeys m t n) =
  intercalate "\n"
  [ unwords
    [ "Monkey"
    , show k
    , ":"
    , show (v ^. items)
    , "-"
    , "Inspects:"
    , show (t I.! k)
    ]
  | (k,v) <- I.toList m
  ]

strip :: String -> String
strip = reverse . dropWhile (/=' '). reverse . dropWhile (/=' ')

parseVal (readMaybe -> Just k) = ValInt k
parseVal "old" = ValOld

empty :: Monkey
empty = Monkey 0 [] (Add, ValInt 0) 0 0 0

runRounds :: Int -> [Monkey] -> Monkeys
runRounds n ms = execState (replicateM n stepRound) (Monkeys m t 0)
  where
    m = I.fromList (zip [0..length ms - 1] ms)
    t = I.fromList (zip [0..length ms - 1] (repeat 0))

monkeyBusiness :: Monkeys -> Integer
monkeyBusiness (Monkeys _ t _)
  = product
  $ take 2
  $ reverse
  $ sort
  $ I.elems t

stepRound :: State Monkeys ()
stepRound = do
  n <- I.size <$> use monkeys
  forM_ [0 .. n - 1] monkeyAround

main :: IO ()
main = do
  asts <- splitOn [""] . lines <$> readFile "11.txt"
  let monkeys = fmap parse asts
  print $ monkeyBusiness (runRounds 20 monkeys)

monkeyAround :: Int -> State Monkeys ()
monkeyAround m = do
  monkey <- (I.! m) <$> use monkeys
  forM_ (monkey ^. items) $ \item -> do
    let
      worryLevel = bored (adjustLevel item (monkey ^. op))
    if worryLevel `mod` (monkey ^. divisible) == 0
      then
        throwItem (monkey ^. ifTrue) worryLevel
      else
        throwItem (monkey ^. ifFalse) worryLevel
  tosses . ix m += fromIntegral (length (monkey ^. items))
  monkeys . ix m . items .= []

throwItem :: Int -> Integer -> State Monkeys ()
throwItem monkey item = monkeys . ix monkey . items %= (++ [item])

bored :: Integer -> Integer
bored lvl = fromIntegral $ floor (fromIntegral lvl / 3.0)

adjustLevel :: Integer -> (Op, Val) -> Integer
adjustLevel old (Add, ValInt x) = old + x
adjustLevel old (Mul, ValInt x) = old * x
adjustLevel old (Mul, ValOld) = old * old
adjustLevel old (Add, ValOld) = old + old

parse :: [String] -> Monkey
parse [ words -> ["Monkey", read . takeWhile isDigit -> monkey :: Integer]
      , (fmap read . splitOn "," . concat . drop 2 . words) -> items :: [Integer]
      , words -> ["Operation:", "new", "=", "old", parseOp -> op, v]
      , words -> ["Test:", "divisible", "by", read -> divisible :: Integer]
      , words -> ["If","true:","throw","to","monkey",read -> ifTrue :: Int]
      , words -> ["If","false:","throw","to","monkey",read -> ifFalse :: Int]
      ] = Monkey
        { _monkey = monkey
        , _items = items
        , _op = (op, parseVal v)
        , _divisible = divisible
        , _ifTrue = ifTrue
        , _ifFalse = ifFalse
        }

parseOp :: String -> Op
parseOp "+" = Add
parseOp "*" = Mul

