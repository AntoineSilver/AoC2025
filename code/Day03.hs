module Day03 where

import           Data.Foldable   (maximumBy)
import           Data.Function   (on)
import           Data.List       (foldl')
import           Data.List.Split

testInput :: [[Integer]]
testInput = [
          [9,8,7,6,5,4,3,2,1,1,1,1,1,1,1],
          [8,1,1,1,1,1,1,1,1,1,1,1,1,1,9],
          [2,3,4,2,3,4,2,3,4,2,3,4,2,7,8],
          [8,1,8,1,8,1,9,1,1,1,1,2,1,1,1]]

-- ANCHOR: in1
readInput :: IO [[Integer]]
readInput = do
    str <- readFile "./input/Day03.txt"
    return $ f <$> lines str
  where
    f :: String -> [Integer]
    f = map (\c -> read [c])
-- ANCHOR_END: in1

-- ANCHOR: max2subnumber
max2subnumber :: [Integer] -> Integer
max2subnumber ds = 10 * dn + dm
  where ids = zip [0..] ds
        (n, dn) = maximumBy order $ init ids
        (m, dm) = maximumBy order $ drop (n+1) ids
        order (a, da) (b, db)
              | ordDigit == EQ = compare b a
              | otherwise      = ordDigit
              where ordDigit = compare da db
-- ANCHOR_END: max2subnumber

-- ANCHOR: out1
solution1 :: IO Integer
solution1 = max2list <$> readInput
  where max2list = sum . map max2subnumber
-- ANCHOR_END: out1

-- ANCHOR: maxNsubnumber
maxNsubnumber :: Int -> [Integer] -> Integer
maxNsubnumber n ds
  | n == 1 = di
  | otherwise = 10^(n-1) * di + maxNsubnumber (n-1) (drop (i+1) ds)
  where nb_ds = length ds
        ids = zip [0..] ds
        (i, di) = maximumBy order $ take (nb_ds - n + 1) ids
        order (a, da) (b, db)
              | ordDigit == EQ = compare b a
              | otherwise      = ordDigit
              where ordDigit = compare da db
-- ANCHOR_END: maxNsubnumber

-- ANCHOR: out2
solution2 :: IO Integer
solution2 = max12list <$> readInput
  where max12list = sum . map (maxNsubnumber 12)
-- ANCHOR_END: out2
