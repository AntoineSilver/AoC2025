module Day03 where

-- [[file:Day03.org::Setup][Setup]]
import Data.List (foldl')
import Data.List.Split
import Data.Foldable (maximumBy)
import Data.Function (on)
-- Setup ends here

-- [[file:Day03.org::Parse Data][Parse Data]]
testInput :: [[Integer]]
testInput = [
          [9,8,7,6,5,4,3,2,1,1,1,1,1,1,1],
          [8,1,1,1,1,1,1,1,1,1,1,1,1,1,9],
          [2,3,4,2,3,4,2,3,4,2,3,4,2,7,8],
          [8,1,8,1,8,1,9,1,1,1,1,2,1,1,1]]

readInput :: IO [[Integer]]
readInput = do
    str <- readFile "./input/Day03.txt"
    return $ f <$> lines str
  where
    f :: String -> [Integer]
    f = map (\c -> read [c])
-- Parse Data ends here

-- [[file:Day03.org::Make largest digit][Make largest digit]]
makeMaxNumbers :: [Integer] -> Integer
makeMaxNumbers ds = 10 * dn + dm
  where ids = zip [0..] ds
        (n, dn) = maximumBy order $ init ids
        (m, dm) = maximumBy order $ drop (n+1) ids
        order (a, da) (b, db)
              | cdadb == EQ = compare b a
              | otherwise = cdadb
              where cdadb = compare da db
-- Make largest digit ends here

-- [[file:Day03.org::Solution 1][Solution 1]]
max2Jolt :: [[Integer]] -> Integer
max2Jolt = sum . map makeMaxNumbers

solution1 = max2Jolt <$> readInput
-- Solution 1 ends here

-- [[file:Day03.org::*Part 2: 12x Jolt!][Part 2: 12x Jolt!:1]]
makeMaxNumbersN :: Int -> [Integer] -> Integer
makeMaxNumbersN n ds
  | n == 1 = di
  | otherwise = 10^(n-1) * di + makeMaxNumbersN (n-1) (drop (i+1) ds) 
  where nb_ds = length ds
        ids = zip [0..] ds
        (i, di) = maximumBy order $ take (nb_ds - n + 1) ids
        order (a, da) (b, db)
              | cdadb == EQ = compare b a
              | otherwise = cdadb
              where cdadb = compare da db
-- Part 2: 12x Jolt!:1 ends here

-- [[file:Day03.org::Solution 2][Solution 2]]
max12Jolt :: [[Integer]] -> Integer
max12Jolt = sum . map (makeMaxNumbersN 12)

solution2 = max12Jolt <$> readInput
-- Solution 2 ends here
