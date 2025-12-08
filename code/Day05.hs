{-# LANGUAGE OverloadedStrings #-}
module Day05 where

-- [[file:Day05.org::Setup][Setup]]
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer
import Data.Either
import Data.Text (Text)
import qualified Data.Text.IO as Txt.IO
import Data.Void (Void)
import GHC.Num (integerFromInt)
import Data.List (sortOn)
-- Setup ends here

-- [[file:Day05.org::Data Structure for Ranges][Data Structure for Ranges]]
data Range = Range !Integer !Integer deriving (Show)
-- Data Structure for Ranges ends here

-- [[file:Day05.org::Parse Data][Parse Data]]
type Parser a = Parsec Void Text a -- Some type aliase for niceness

rangeParser :: Parser Range
rangeParser = do
  x <- decimal
  char '-'
  y <- decimal
  return $ Range x y

inputParser :: Parser ([Range], [Integer])
inputParser = do
  rs <- rangeParser `sepEndBy1` newline
  eol
  ids <- decimal `sepEndBy1` newline
  eof
  return (rs, ids)

readInput :: IO ([Range], [Integer])
readInput = do
  input <- Txt.IO.readFile "./input/Day05.txt"
  case parse inputParser "Day05.txt" input of
    Left err  -> do {putStr (errorBundlePretty err); return ([],[]);}
    Right res -> return res
-- Parse Data ends here

-- [[file:Day05.org::Solution 1][Solution 1]]
isIn :: Integer -> Range -> Bool
isIn i (Range n m) = n <= i && i <= m

isFresh :: [Range] -> Integer -> Bool
isFresh rs i = foldr ((||) . isIn i) False rs

countFresh :: [Range] -> [Integer] -> Integer
countFresh rs ids = integerFromInt $ length $ filter (isFresh rs) ids

solution1 :: IO Integer
solution1 = uncurry countFresh <$> readInput
-- Solution 1 ends here

-- [[file:Day05.org::Solution 2][Solution 2]]
rangeSize :: Range -> Integer
rangeSize (Range n m) = m - n + 1

countAllFresh :: [Range] -> Integer
countAllFresh = countAllFreshFromSorted . sortOn rangeSize

countAllFreshFromSorted :: [Range] -> Integer
countAllFreshFromSorted [] = 0 
countAllFreshFromSorted (r:rs) = f r rs + countAllFreshFromSorted rs
  where
    -- f r rs: gives the number of elements in r that are not in rs this is done
    --         by making r smaller for each r' in rs since (r:rs) is sorted by
    --         increasing size, we can garanty that r \ r' is again a range.
    f r [] = rangeSize r
    f r (r':rs) = if newN <= newM then f newR rs else 0
      where Range n m = r
            Range n' m' = r'
            newN = if n `isIn` r' then m'+1 else n 
            newM = if m `isIn` r' then n'-1 else m 
            newR = Range newN newM


solution2 :: IO Integer
solution2 = f <$> readInput
  where f (rs, ids) = countAllFresh $ sortOn rangeSize rs
-- Solution 2 ends here
