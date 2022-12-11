{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
module Main where

import Control.Lens        ( use, (%=), (+=), (.=), makeLenses )
import Control.Monad.State
    ( forM_,
      when,
      execStateT,
      MonadIO(liftIO),
      MonadState(get),
      StateT )

type Clock = Int
type Value = Int

data Op
  = Add Int
  | NoOp
  deriving (Show, Eq)

data CPU
  = CPU
  { _clock    :: Clock
  , _x        :: Int
  , _strength :: [Int]
  , _delayed  :: [Delayed]
  } deriving (Show, Eq)

data Delayed = Delayed Op Clock
  deriving (Show, Eq)

$(makeLenses 'CPU)

main :: IO ()
main = do
  ops <- fmap parseOp . lines <$> readFile "10.txt"
  print =<< eval ops

parseOp :: String -> Op
parseOp "noop" = NoOp
parseOp (words -> [x,y]) = Add (read y)

applyOp :: Int -> Op -> Int
applyOp y NoOp = y
applyOp y (Add x) = x + y

eval :: [Op] -> IO Int
eval ops = sum . _strength <$> execStateT (runMachine ops) emptyCPU

emptyCPU :: CPU
emptyCPU = CPU 1 1 [] []

runMachine :: [Op] -> StateT CPU IO ()
runMachine ops = forM_ ops go
  where
    go :: Op -> StateT CPU IO ()
    go NoOp = do
      drawPixel
      strengthen
      processDelayed
      clock += 1

    go (Add n) = do
      drawPixel
      strengthen
      processDelayed
      c <- use clock
      clock += 1
      drawPixel
      delayed %= (++ [Delayed (Add n) (c + 1)])
      strengthen
      processDelayed
      clock += 1

strengthen :: StateT CPU IO ()
strengthen = do
  c <- use clock
  v <- use x
  when (c `elem` [20,60,100,140,180,220]) $ do
    strength %= ((c * v):)

processDelayed :: StateT CPU IO ()
processDelayed = do
  c <- use clock
  use delayed >>= \case
    [] -> pure ()
    Delayed (Add k) c' : ds -> do
      when (c == c') $ do
        x %= flip applyOp (Add k)
        delayed .= ds

drawPixel :: StateT CPU IO ()
drawPixel = do
  c <- use clock
  v <- use x
  liftIO $ do
    when (c - 1 `elem` [40,80,120,160,200,240]) (putChar '\n')
    if ((c - 1) `mod` 40) `elem` [v - 1 .. v + 1]
      then putChar '#'
      else putChar '.'

