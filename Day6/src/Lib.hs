module Lib
  where

import Data.List.Split
import qualified Data.Set as Set

type Index = Int
type State = (Set.Set [Int], [Int], Int)

toNumbers :: String -> [Int]
toNumbers = map (read :: String -> Int) . words

part1 :: String -> Int
part1 input = getCycles $ getLoop (Set.empty, toNumbers input, 0)

part2 :: String -> Int
part2 input = sndCycle
  where
    (_, ns, _) = getLoop (Set.empty, toNumbers input, 0)
    (_, _, sndCycle) = getLoop (Set.empty, ns, 0)

getCycles :: State -> Int
getCycles (_, _, x) = x

getLoop :: State -> State
getLoop = until isDone distribute
  where
    isDone (s, l, i) = Set.member l s
    distribute (s, l, i) = (Set.insert l s, update l, succ i)

update :: [Int] -> [Int]
update l = map (snd . addNewBlocks . empty) indexed
  where
    len = length l
    indexed = zip [0..] l
    selected = foldr larger (last indexed) indexed
    empty (i, x) = if i == fst selected then (i, 0) else (i, x)
    addNewBlocks (i, x) = (i, x + blocks len selected i)
    larger (i, x) (j, y) = if x >= y then (i, x) else (j, y)

blocks :: Int -> (Index, Int) -> Index -> Int
-- given selected bank, calculate increasing block numbers for a bank of another index
blocks len (si, bs) i = length $ takeWhile (<= bs) $ filter (/= 0) [bi, bi + len ..]
  where
    bi = (len + (i - si)) `mod` len

