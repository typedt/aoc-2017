module Lib
  where

part1 :: Int -> Int
part1 n = distance $ foldl walk start ms
  where
    start = startCoords n
    ms = take diff (moves sqr)
    sqr = lastSqrRoot n
    diff = n - sqr * sqr
    distance (x, y) = abs x + abs y

allSqrRoots :: Int -> [Int]
allSqrRoots n = takeWhile ((<= n) . sqr) [3, 5..]
  where
    sqr :: Int -> Int
    sqr x = x * x

lastSqrRoot :: Int -> Int
lastSqrRoot = last . allSqrRoots

startCoords :: Int -> (Int, Int)
startCoords n = (x, negate x)
  where x = length $ allSqrRoots n

moves :: Int -> [(Int, Int)]
moves sqrRoot= firstEast ++ thenNorth ++ thenWest ++ thenSouth ++ lastEast
  where
    repeat1 = replicate $ fromIntegral sqrRoot
    repeat2 = replicate $ fromIntegral (sqrRoot + 1)
    firstEast = [(1, 0)]
    thenNorth = repeat1 (0, 1)
    thenWest = repeat2 (-1, 0)
    thenSouth = repeat2 (0, -1)
    lastEast = repeat2 (1, 0)

walk :: (Int, Int) -> (Int, Int) -> (Int, Int)
walk (x, y) (i, j) = (x + i, y + j)
