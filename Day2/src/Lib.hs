module Lib
  where

import Data.List
import Data.List.Split

part1 :: String -> Int
part1 = foldl (+) 0 . map checksum . lines
  where
    checksum = difOfHeadLast . sort . toNumbers
    difOfHeadLast xs = last xs - head xs

part2 :: String -> Int
part2 = foldl (+) 0 . map checksum . lines
  where
    checksum :: String -> Int
    checksum = myDiv. head . dropWhile (not . divisable) . pairs . toNumbers

pairs :: [Int] -> [(Int, Int)]
pairs l = [(i, j) | (i: js) <- tails l, j <- js, i /= j]

divisable :: (Int, Int) -> Bool
divisable (x, y)
  | x == y = False
  | x > y = x `rem` y == 0
  | x < y = y `rem` x == 0

myDiv :: (Int, Int) -> Int
myDiv (x, y)
  | x < y = y `quot` x
  | x >= y = x `quot` y

toNumbers :: String -> [Int]
toNumbers = map readInt . splitOneOf " \t"
  where
    readInt = read :: String -> Int
