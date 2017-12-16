module Lib
  where

import Data.Char
import Data.List
import Data.Either
import Data.Maybe
import qualified Data.Map as M
import qualified Data.Tree as T
import Control.Applicative
import Text.Parsec
import Text.Parsec.String (Parser)

part1 :: String -> String
part1 = head . head . filter ((== 1) . length) . group . sort . words . filter isLetterOrSpace
  where
    isLetterOrSpace = liftA2 (||) isLetter isSpace

part2 :: String -> Int
part2 = correctWeight . liftA2 toTree parseInput part1

data PNode = PNode {getName :: String, getWeight :: Int, getSubNodes :: [String]}
  deriving Show

parseInput :: String -> M.Map String (Int, [String])
parseInput = M.fromList . map (kv . unwrap. parse pNode "") . lines
  where
    kv = liftA2 (,) getName $ liftA2 (,) getWeight getSubNodes
    unwrap = either (const undefined) id

pNode :: Parser PNode
pNode = PNode <$> word <*> weight <*> option [] subNodes
  where
    word = lexeme $ many1 letter
    weight = lexeme $ between (char '(') (char ')') number
    subNodes = arrow *> lexeme word  `sepBy` (lexeme . char) ','
    number = (read :: String -> Int) <$> many1 digit
    arrow = lexeme $ string "->"
    lexeme :: Parser a -> Parser a
    lexeme p = p <* spaces

data TNode = TNode {getLabel :: String, getSumWeight :: Int}
  deriving Show

instance Eq TNode where
  (==) x y = getSumWeight x == getSumWeight y

toTree :: M.Map String (Int, [String]) -> String -> T.Tree TNode
toTree m r = T.Node node forest
  where
    node = TNode r sumWeight
    forest = map (toTree m) subNodes
    (selfWeight, subNodes) = fromJust $ M.lookup r m
    sumWeight = foldr ((+) . getSumWeight) selfWeight $ map T.rootLabel forest

correctWeight :: T.Tree TNode -> Int
correctWeight tree =
  let diff = listToMaybe . filter ((== 1) . length) . groupBy eq . sortOn weight
      eq x y = weight x == weight y
      weight = getSumWeight . T.rootLabel
      correct t = (weight $ head $ f \\ [t]) - (sum . map weight . T.subForest) t
      f = T.subForest tree
      (this:_) = fromJust $ diff f
      maybeNext = diff $ T.subForest this
  in case maybeNext of
       Nothing -> correct this
       Just (next:_) -> correctWeight next

