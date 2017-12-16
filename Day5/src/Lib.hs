module Lib
  where

import qualified Data.Map.Strict as Map

type Index = Int

part1 :: String -> Int
part1 input = count (toMap input) 0 0
  where
    maxIndex = (length . lines) input
    pairs = toIndexedNumbers input
    count :: Map.Map Index Int -> Index -> Int -> Int
    count m i s = if isOut then s else count m' i' s'
      where
        isOut = i >= maxIndex
        i' = i + m Map.! i
        s' = succ s
        m' = Map.adjust succ i m

toIndexedNumbers :: String -> [(Index, Int)]
toIndexedNumbers = zip [0..] . map (read :: String -> Int) . lines

toMap :: String -> Map.Map Index Int
toMap = Map.fromList . toIndexedNumbers

part2 :: String -> Int
part2 input = count' (toMap input) 0 0
  where
    maxIndex = (length . lines) input
    count' :: Map.Map Index Int -> Index -> Int -> Int
    count' m i steps = if isOut then steps else count' m' i' steps'
      where
        isOut = i >= maxIndex
        i' = i + m Map.! i
        steps' = succ steps
        m' = Map.adjust updateOffset i m
        updateOffset x = if x >= 3 then pred x else succ x

