module Lib
   where

import Data.List
import Data.Char

part1 :: String -> Int
part1 s = sumOfEqualDigits $ zip s $ rotate 1 s

part2 :: String -> Int
part2 s = sumOfEqualDigits $ zip s $ rotate n s
  where
    n = length s `quot` 2

sumOfEqualDigits :: [(Char, Char)] -> Int
sumOfEqualDigits = foldl (+) 0 . map (digitToInt . fst) . filter (uncurry (==))

rotate :: Int -> String -> String
rotate n s = drop n s ++ take n s
