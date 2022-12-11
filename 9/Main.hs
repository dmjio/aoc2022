{-# LANGUAGE ViewPatterns #-}
import qualified Data.IntMap.Strict as IM
import Data.List

main :: IO ()
main = do
  cmds <- fmap parse . lines <$> readFile "9.txt"
  print $ solution (padCmds cmds)

  print $ solution2 (padCmds cmds)

padCmds :: [(Dir,Int)] -> [(Dir,Int)]
padCmds xs = xs >>= \(d,n) -> replicate n (d,1)

parse :: String -> (Dir, Int)
parse (words -> [read -> dir, read -> x]) = (dir, x)

data Dir
  = U
  | D
  | L
  | R
  deriving (Read, Show)

type Head = (Int,Int)
type Tail = (Int,Int)

s = (0,0)

solution :: [(Dir, Int)] -> Int
solution cmds = length $ nub $ fmap snd $ scanl' go (s,s) cmds
  where
    go (h,t) (dir, n) = step
      where
          step =
            let
              h' = adjustHead (dir,n) h
              t' = shiftTail h' t
            in
              (h',t')

s' :: (Head, Tails)
s' = (s, IM.fromList (zip [1..9] (repeat [s])))

type Tails = IM.IntMap [Tail]

solution2 :: [(Dir, Int)] -> Int
solution2 cmds = length $ nub $ (IM.! 9) $ snd $ foldl' go s' cmds
  where
    go :: (Head, Tails) -> (Dir, Int) -> (Head, Tails)
    go (h, m) (dir, n) = step
      where
          step =
            let
              h' = adjustHead (dir,n) h
              t' = shiftTails h' m
            in
              (h',t')

shiftTails :: Head -> IM.IntMap [Tail] -> IM.IntMap [Tail]
shiftTails = go 1
  where
    go :: Int -> Head -> IM.IntMap [Tail] -> IM.IntMap [Tail]
    go 10 _ m = m
    go k h m =
      let
        t = shiftTail h (head (m IM.! k))
      in
        go (k + 1) t (ins k [t] m)

ins :: Int -> [Tail] -> IM.IntMap [Tail] -> IM.IntMap [Tail]
ins = IM.insertWith (\x y -> if head x == head y then y else x ++ y)


adjustHead :: (Dir, Int) -> Head -> Head
adjustHead (U, n) (x,y) = (x, y + n)
adjustHead (D, n) (x,y) = (x, y - n)
adjustHead (L, n) (x,y) = (x - n, y)
adjustHead (R, n) (x,y) = (x + n, y)

shiftTail :: Head -> Tail -> Tail
shiftTail h@(x,y) t@(x',y')
  | x /= x' && y /= y' && abs (x - x') + abs (y - y') /= 2
  = shiftDiagonally h t
  | x - x' ==  2 = (x' + 1, y')
  | x - x' == -2 = (x' - 1, y')
  | y - y' ==  2 = (x', y' + 1)
  | y - y' == -2 = (x', y' - 1)
  | otherwise = t

shiftDiagonally (x,y) (x',y')
  | x > x' && y > y' = (x' + 1, y' + 1)
  | x > x' && y < y' = (x' + 1, y' - 1)
  | x < x' && y > y' = (x' - 1, y' + 1)
  | x < x' && y < y' = (x' - 1, y' - 1)

test = shiftTail (2,1) (1,3)
