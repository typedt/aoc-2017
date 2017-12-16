module Lib
  where

part1 :: String -> Int
part1 = sum . groups . processed

part2 :: String -> Int
part2 = garbageCount . processed

processed :: String -> State
processed = foldl process (State False False [] [] 0)

data State = State {isInGarbage :: Bool, isAfterBang :: Bool, stack :: [Char],
                     groups :: [Int], garbageCount :: Int} deriving Show

process :: State -> Char -> State
process gs@(State ig ib s g gc) c = if ig then updateIg gs else updateNg gs
  where
    updateIg gs@(State True ib s g gc) = if ib then (State True False s g gc) else updateIgNb gs
    updateIgNb (State True False s g gc) = case c of
                                      '!' -> State ig True s g gc
                                      '>' -> State False False s g gc
                                      _   -> State ig False s g (gc + 1)
    updateNg (State False False s g gc) = case c of
                                    '{' -> State False False (c : s) g gc
                                    '}' -> State False False (init s) ((length s) : g) gc
                                    ',' -> State False False s g gc
                                    '<' -> State True False s g gc

