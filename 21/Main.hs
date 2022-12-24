module Main where

import           Data.List.Split ( splitOn )
import           Numeric         ( showFFloat )
import qualified Data.Map.Strict as M ( (!), fromList, Map )

main :: IO ()
main = do
  m <- build . fmap parse . lines <$> readFile "21.txt"

  putStrLn "Part 1"
  print $ round (interpret m "root")

  putStrLn "Part 2"
  let
    upperBound = fromIntegral (maxBound :: Int)
    lowerBound = 0
  putStrLn $ prettyFloat (search m lowerBound upperBound)
    where
      prettyFloat x = showFFloat Nothing x mempty

search :: M.Map Name (Val Double) -> Double -> Double -> Double
search m l u = do
  let mid = (u + l) / 2
  case findHuman m mid of
    LT -> search m l mid
    EQ -> mid
    GT -> search m mid u

build :: [Val a] -> M.Map Name (Val a)
build vals = M.fromList (unwrap <$> vals)
  where
    unwrap v@(Val n _)      = (n, v)
    unwrap v@(Comp n _ _ _) = (n, v)

interpret :: M.Map Name (Val Double) -> Name -> Double
interpret m key =
  case m M.! key of
    Val name v -> v
    Comp name f l r -> f (interpret m l) (interpret m r)

findHuman
  :: M.Map Name (Val Double)
  -> Double
  -> Ordering
findHuman m x = compare (go l) (go r)
  where
    Comp "root" _ l r = m M.! "root"

    go :: Name -> Double
    go key =
      case m M.! key of
        Val "humn" _ -> x
        Val name v -> v
        Comp name f l r -> f (go l) (go r)

parse :: (Fractional a, Read a, Num a) => String -> Val a
parse xs =
  case splitOn ":" xs of
    [name, val] ->
      case words val of
        [x] -> Val name (read x)
        [l,"*",r] -> Comp name (*) l r
        [l,"+",r] -> Comp name (+) l r
        [l,"/",r] -> Comp name (/) l r
        [l,"-",r] -> Comp name (-) l r

type Name = String

data Val a
  = Val Name a
  | Comp Name (a -> a -> a) Name Name
