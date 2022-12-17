{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import           Control.Arrow       ( Arrow(first) )
import           Control.Lens        ( (^.), to, use, (%=), makeLenses )
import           Control.Monad       ( forM_, when )
import           Control.Monad.State ( State, execState )
import           Data.Char           ( ord )
import           Data.Map            ( Map )
import qualified Data.Map.Strict     as M ( fromList, (!), filter, keys, lookup, toList, insert )
import           Data.Maybe          ( isJust )
import           Data.OrdPSQ         ( OrdPSQ )
import qualified Data.OrdPSQ         as PSQ ( delete, insert, findMin, fromList )
import           Data.Set            ( Set )
import qualified Data.Set            as S ( insert, notMember, singleton )
import           Data.Tuple          ( swap )

type Dijkstra = State M
type Grid     = Map (Int,Int) Char
type Coord    = (Int,Int)
type Weights  = Map (Int, Int) Int
type Weight   = Int

data M
  = M
  { _grid :: Grid
  , _seen :: Set Coord
  , _weights :: Weights
  , _queue :: OrdPSQ Coord Weight Char
  }

$(makeLenses ''M)

main :: IO ()
main = do
  string <- readFile "12.txt"
  let
    ls = lines string
    x = length ls
    y = length (head ls)
    grid = readGrid (concat ls) (x,y)

  let
    finish = (M.! 'E') $ M.fromList $ fmap swap $ M.toList grid
    start  = (M.! 'S') $ M.fromList $ fmap swap $ M.toList grid
    run s = runDijkstra s finish grid dijkstra ^. weights . to (M.! finish)

  putStrLn "Part 1"
  print (run start)

  putStrLn "Part 2"
  print
    $ minimum
    $ fmap run
    $ start : M.keys (M.filter (=='a') grid)

readGrid :: String -> (Int,Int) -> Grid
readGrid str (x,y) =
  let
    coords = (,) <$> [0 .. x - 1] <*> [0 .. y - 1]
  in
    M.fromList (first swap <$> zip coords str)

getVal :: Grid -> Coord -> Int
getVal grid m =
  case grid M.! m of
    'E' -> ord 'z'
    'S' -> ord 'a'
    x   -> ord x

neighbors :: Coord -> Dijkstra [Coord]
neighbors c = neighbs c <$> use grid
  where
    neighbs :: Coord -> Grid -> [Coord]
    neighbs coord@(x,y) grid =
      [ c
      | c <- coords
      , and
        [ isJust (M.lookup c grid)
        , getVal grid c - getVal grid coord <= 1
        ]
      ] where
          coords =
            [ (x+1,y)
            , (x-1,y)
            , (x,y+1)
            , (x,y-1)
            ]

runDijkstra :: Coord -> Coord -> Grid -> Dijkstra a -> M
runDijkstra start finish grid dijk = execState dijk s
    where
      s = M grid seen weights queue
      seen = S.singleton start
      weights
        = M.insert start 0
        $ M.fromList
        $ zip (M.keys grid) (repeat maxBound)
      queue =
        PSQ.fromList
        [ case k of
            coord
              | coord == start -> (start, 0, v)
              | otherwise -> (coord, maxBound, v)
        | (k,v) <- M.toList grid
        ]

extractMin :: Dijkstra (Maybe Coord)
extractMin = do
  s <- use seen
  q <- use queue
  PSQ.findMin <$> use queue >>= \case
    Nothing -> pure Nothing
    Just v@(coord, w, _) -> do
      if w == maxBound
        then pure Nothing
        else do
          seen %= S.insert coord
          queue %= PSQ.delete coord
          pure (Just coord)

dijkstra :: Dijkstra ()
dijkstra = mapM_ next =<< extractMin
  where
    next u = do
      relaxEdges u =<< neighbors u
      dijkstra

    relaxEdges u vs = do
      s <- use seen
      forM_ vs $ \v -> do
        when (v `S.notMember` s) $ do
          w <- use weights
          let weight = min (w M.! v) ((w M.! u) + 1)
          c <- (M.! v) <$> use grid
          weights %= M.insert v weight
          queue %= PSQ.insert v weight c
