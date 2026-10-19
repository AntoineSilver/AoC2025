module Day02 where

import           Data.List       (foldl')
import           Data.List.Split

-- ANCHOR: types
data Range = Range Integer Integer deriving (Show)
-- ANCHOR_END: types

-- ANCHOR: read
readInput :: IO [Range]
readInput = do
    str <- readFile "./input/Day02.txt"
    return $ f <$> splitOn "," str
  where
    f :: String -> Range
    f str = Range (read start) (read end)
      where
        l = splitOn "-" str
        start = head l
        end = last l
-- ANCHOR_END: read

-- ANCHOR: decomposeRange
decomposeRange :: Range -> [Range]
decomposeRange (Range n m)
  | dn == dm = [Range n m]
  | otherwise = newRanges
  where dn = length $ show n
        dm = length $ show m
        newRanges = Range n (10^dn - 1)
                    : Range (10^(dm-1)) m
                    : [Range (10^d) (10^(d+1)-1) | d <- [dn..(dm-2)] ]
-- ANCHOR_END: decomposeRange


-- ANCHOR: sum2Repetitions
sum2Repetitions :: Range -> Integer
sum2Repetitions (Range n m)
  | dn == dm = if even dn then kk*(b-a'+1)*(b+a') `div` 2 else 0
  | otherwise = sum2Repetitions_fromList $ decomposeRange (Range n m)
  where dn = length $ show n
        dm = length $ show m
        -- For equal case
        k  = dn `div` 2
        kk = 10^k + 1
        a  = n `div` kk
        b  = m `div` kk
        a'  = if (n `mod` kk) == 0 then a else a+1

sum2Repetitions_fromList :: [Range] -> Integer
sum2Repetitions_fromList = sum . map sum2Repetitions
-- ANCHOR_END: sum2Repetitions

-- ANCHOR: out1
solution1 :: IO Integer
solution1 = sum2Repetitions_fromList <$> readInput
-- ANCHOR_END: out1

-- ANCHOR: sumRRepetitions
sumRRepetitions :: Integer -> Range -> Integer
sumRRepetitions r (Range n m)
  | dn == dm = if (dn `mod` r) == 0 then kk*((b-a'+1)*(b+a') `div` 2) else 0
  | otherwise = sumRRepetitions_fromList r $ decomposeRange (Range n m)
  where dn = toInteger $ length $ show n
        dm = toInteger $ length $ show m
        -- For equal case
        k  = dn `div` r
        kk = (10^(r * k) - 1) `div` (10^k - 1)
        a  = n `div` kk
        b  = m `div` kk
        a'  = if (n `mod` kk) == 0 then a else a+1

sumRRepetitions_fromList :: Integer -> [Range] -> Integer
sumRRepetitions_fromList r = sum . map (sumRRepetitions r)
-- ANCHOR_END: sumRRepetitions

-- ANCHOR: helper
primeFactors :: Integer -> [Integer]
primeFactors n = step (abs n) 2
  where
    step x p
      | x <= 1         = []
      | (p * p) > x    = [x] -- no more factors
      | x `mod` p == 0 = p : step (strip p x) (p+1)
      | otherwise      = step x (p+1)

    strip p x -- remove all factors of p from x
      | x `mod` p == 0 = strip p (x `div` p)
      | otherwise      = x

bitStrings :: Int -> [[Integer]]
bitStrings 0 = [[]]
bitStrings j
  | j < 0 = error "Bit-strings must have positive length"
  | j > 0 = z ++ o
  where z = map (0:) $ bitStrings (j-1)
        o = map (1:) $ bitStrings (j-1)
-- ANCHOR_END: helper

-- ANCHOR: sumAllRepetitions
sumAllRepetitions :: Range -> Integer
sumAllRepetitions range
  | dn == dm && dn == 1 = 0 -- No 1 digit repetitions
  | dn == dm = sum $ zipWith (*) signs $ map (`sumRRepetitions` range) rs
  | otherwise = sumAllRepetitions_fromList $ decomposeRange (Range n m)
  where (Range n m) = range
        dn = toInteger $ length $ show n
        dm = toInteger $ length $ show m
        -- For equal case
        ps   = primeFactors dn
        bits = tail $ bitStrings $ length ps
               -- ^ the first one is ignored (all zeros)
        rs    = map (product . zipWith (^) ps ) bits
        signs = map (\ x -> (-1)^(1 + sum x)) bits

sumAllRepetitions_fromList :: [Range] -> Integer
sumAllRepetitions_fromList = sum . map sumAllRepetitions
-- ANCHOR_END: sumAllRepetitions

-- ANCHOR: out2
solution2 :: IO Integer
solution2 = sumAllRepetitions_fromList <$> readInput
-- ANCHOR_END: out2
