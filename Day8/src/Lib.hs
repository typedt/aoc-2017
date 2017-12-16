module Lib
  where

import Text.Parsec
import Text.Parsec.String (Parser)
import qualified Data.Map.Strict as M
import Data.List (maximum)

part1 :: String -> Int
part1 = maximum . M.elems . foldl applyIns M.empty . map (unEither . parse parser "") . lines

part2 :: String -> Int
part2 = snd . foldl applyIns' (M.empty, minBound :: Int) . map (unEither . parse parser "") . lines

data Instruction = Ins {getReg :: String, getOp :: Int -> Int, getPred :: (String, Int -> Bool)}

parser :: Parser Instruction
parser = Ins <$> reg <*> op <*> predicate
  where
    reg = lexeme $ many1 letter
    op = flip <$> lexeme (inc <|> dec) <*> lexeme number
      where
        inc = (+) <$ string "inc"
        dec = (-) <$ string "dec"
    predicate = pfx *> ((,) <$> reg <*> comp)
      where
        pfx = lexeme (string "if")
        comp = flip <$> lexeme (try le <|> lt <|> try ge <|> gt <|> eq <|> ne) <*> number
        lt = (<) <$ char '<'
        gt = (>) <$ char '>'
        le = (<=) <$ string "<="
        ge = (>=) <$ string ">="
        eq = (==) <$ string "=="
        ne = (/=) <$ string "!="
    lexeme :: Parser a -> Parser a
    lexeme = flip (<*) spaces
    number = (read :: String -> Int) <$> strNumber
      where
        strNumber = (++) <$> option "" (string "-") <*> many1 digit

unEither :: Either ParseError a -> a
unEither = either (const undefined) id

applyIns :: M.Map String Int -> Instruction -> M.Map String Int
applyIns m (Ins this op (that, p)) = if satisfies then M.alter f this m else m
  where
    f (Just v) = Just $ op v
    f Nothing = Just $ op 0
    satisfies = p (M.findWithDefault 0 that m)

applyIns' :: (M.Map String Int, Int) -> Instruction -> (M.Map String Int, Int)
applyIns' (m, h) (Ins this op (that, p)) = if satisfies then (m', h') else (m, h)
  where
    satisfies = p (M.findWithDefault 0  that m)
    h' = if new > h then new else h
    m' = M.alter f this m
    value = M.lookup this m
    new = case value of
            Just v -> op v
            Nothing -> op 0
    f _ = Just new
