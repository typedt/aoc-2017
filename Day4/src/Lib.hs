module Lib
  where

import Data.List

part1 :: String -> Int
part1 = length . filter isValid . lines
  where
    isValid line = (length $ dedup (words line)) == length (words line)

part2 :: String -> Int
part2 = length . filter isValid . lines
  where
    isValid line = (length $ dedup xs) == length xs
      where
        xs = map sort $ words line

dedup :: (Ord a) => [a] -> [a]
dedup = map head . group . sort

