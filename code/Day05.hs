{-# LANGUAGE OverloadedStrings #-}
module Day05 where

import           Data.Either
import           Data.List                  (sortOn)
import           Data.Text                  (Text)
import qualified Data.Text.IO               as Txt.IO
import           Data.Void                  (Void)
import           GHC.Num                    (integerFromInt)
import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Text.Megaparsec.Char.Lexer

-- ANCHOR: range
data Range = Range !Integer !Integer | Empty deriving (Eq, Show)
-- ANCHOR_END: range

-- ANCHOR: parserType
type Parser a = Parsec Void Text a
  -- Void: the type of the error (Void = no custom error management)
  -- Text: the type of the input
  -- a: the type of the output
-- ANCHOR_END: parserType

-- ANCHOR: rangeParser
rangeParser :: Parser Range
rangeParser = do
  x <- decimal
  char '-'
  y <- decimal
  return $ Range x y
-- ANCHOR_END: rangeParser

-- ANCHOR: inputParser
inputParser :: Parser ([Range], [Integer])
inputParser = do
  rs <- rangeParser `sepEndBy1` newline
  eol
  ids <- decimal `sepEndBy1` newline
  eof
  return (rs, ids)
-- ANCHOR_END: inputParser

-- ANCHOR: readInput
readInput :: IO ([Range], [Integer])
readInput = do
  input <- Txt.IO.readFile "./input/Day05.txt"
  case parse inputParser "Day05.txt" input of
    Left err  -> do {putStr (errorBundlePretty err); return ([],[]);}
    Right res -> return res
-- ANCHOR_END: readInput

-- ANCHOR: isIn
isIn :: Integer -> Range -> Bool
isIn _ Empty       = False
isIn i (Range n m) = n <= i && i <= m

isInAny :: Integer -> [Range] -> Bool
isInAny i = foldr ((||) . isIn i) False
-- alternative: any (isIn i)
-- ANCHOR_END: isIn

-- ANCHOR: solution1
countIncluded :: [Range] -> [Integer] -> Integer
countIncluded rs = foldl' (\n x -> if p x then n + 1 else n) 0
  where p i = i `isInAny` rs

solution1 :: IO Integer
solution1 = uncurry countIncluded <$> readInput
-- ANCHOR_END: solution1

-- ANCHOR: cutRange
rangeSize :: Range -> Integer
rangeSize Empty       = 0
rangeSize (Range n m) = m - n + 1

-- unsafe: assumes size r < size r'
cutRange :: Range -> Range -> Range
cutRange Empty _ = Empty
cutRange r Empty = r -- Should be forbidden really...
cutRange r r'
      | rangeSize r <= rangeSize r' = newR
      | otherwise                   = error "Can only cut ranges if ordered"
      where Range n m = r
            Range n' m' = r'
            newN = if n `isIn` r' then m'+1 else n
            newM = if m `isIn` r' then n'-1 else m
            newR = if newN <= newM then Range newN newM else Empty
-- ANCHOR_END: cutRange

-- ANCHOR: solution2
totalRangeSize :: [Range] -> Integer
totalRangeSize = totalRangeSizeFromSorted . sortOn rangeSize

totalRangeSizeFromSorted :: [Range] -> Integer
totalRangeSizeFromSorted [] = 0
totalRangeSizeFromSorted (r:rs) = f r rs + totalRangeSizeFromSorted rs
  where
    f r [] = rangeSize r
    f r (r':rs)
      | newR == Empty = 0
      | otherwise     = f newR rs
      where newR = cutRange r r'


solution2 :: IO Integer
solution2 = f <$> readInput
  where f (rs, ids) = totalRangeSize $ sortOn rangeSize rs
-- ANCHOR_END: solution2
