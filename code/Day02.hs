module Day02 where

-- [[file:Day02.org::Setup][Setup]]
import Data.List (foldl')
import Data.List.Split
-- Setup ends here

-- [[file:Day02.org::Data Structure for Ranges][Data Structure for Ranges]]
data Range = Range !Integer !Integer deriving (Show)
-- Data Structure for Ranges ends here

-- [[file:Day02.org::Read Input][Read Input]]
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
-- Read Input ends here

-- [[file:Day02.org::Number of Fake IDs in a range][Number of Fake IDs in a range]]
decomposeRange :: Range -> [Range]
decomposeRange (Range n m)
  | dn == dm = [Range n m]
  | otherwise = newRanges
  where dn = length $ show n
        dm = length $ show m
        newRanges = Range n (10^dn - 1)
                    : Range (10^(dm-1)) m
                    : [Range (10^d) (10^(d+1)-1) | d <- [dn..(dm-2)] ]

sumFakeIDs :: [Range] -> Integer
sumFakeIDs = sum . map _sumFakeIDs

_sumFakeIDs :: Range -> Integer
_sumFakeIDs (Range n m)
  | dn == dm = if even dn then kk*(b-a'+1)*(b+a') `div` 2 else 0
  | otherwise = sumFakeIDs $ decomposeRange (Range n m)
  where dn = length $ show n
        dm = length $ show m
        -- For equal case
        k  = dn `div` 2
        kk = 10^k + 1
        a  = n `div` kk
        b  = m `div` kk
        a'  = if (n `mod` kk) == 0 then a else a+1
-- Number of Fake IDs in a range ends here

-- [[file:Day02.org::Solution 1][Solution 1]]
solution1 :: IO Integer
solution1 = sumFakeIDs <$> readInput
-- Solution 1 ends here

-- [[file:Day02.org::Sum r-repetitions][Sum r-repetitions]]
sumRRepetitions :: Integer -> Range -> Integer
sumRRepetitions r (Range n m)
  | dn == dm = if (dn `mod` r) == 0 then kk*((b-a'+1)*(b+a') `div` 2) else 0
  | otherwise = error "Range is not of a single digit number!"
  where dn = toInteger $ length $ show n
        dm = toInteger $ length $ show m
        k  = dn `div` r
        kk = sum [10^(i * k) | i <- [0..(r-1)]]
        a  = n `div` kk
        b  = m `div` kk
        a'  = if (n `mod` kk) == 0 then a else a+1
-- Sum r-repetitions ends here

-- [[file:Day02.org::Prime Decomposition][Prime Decomposition]]
-- Source - https://stackoverflow.com/a
-- Posted by Frank Schmitt, modified by community. See post 'Timeline' for change history
-- Retrieved 2025-12-04, License - CC BY-SA 3.0
primeFactors n =
  case factors of
    [] -> [n]
    _  -> factors ++ primeFactors (n `div` head factors)
  where factors = take 1 $ filter (\x -> (n `mod` x) == 0) [2 .. n-1]

uniquePrimeFactors n = map snd $ filter (uncurry (/=)) $ zip (0:ps) ps
  where ps = primeFactors n
-- Prime Decomposition ends here

-- [[file:Day02.org::Bit-strings][Bit-strings]]
bitStrings 0 = [[]]
bitStrings j
  | j < 0 = error "Bit-strings must have positive length"
  | j > 0 = z ++ o
  where z = map (0:) $ bitStrings (j-1)
        o = map (1:) $ bitStrings (j-1)
-- Bit-strings ends here

-- [[file:Day02.org::Sum all repetitions][Sum all repetitions]]
sumAllRepetitions :: Range -> Integer
sumAllRepetitions range
  | dn == dm && dn == 1 = 0 -- No fake 1 digit indices
  | dn == dm = sum $ zipWith (*) signs $ map (`sumRRepetitions` range) rs
  | otherwise = error "Range is not of a single digit number!"
  where (Range n m) = range
        dn = toInteger $ length $ show n
        dm = toInteger $ length $ show m
        ps = uniquePrimeFactors dn
        bits = tail $ bitStrings $ length ps -- the first one is ignored
        rs = map (product . zipWith (^) ps ) bits
        signs = map (\ x -> (-1)^(1 + sum x)) bits
-- Sum all repetitions ends here

-- [[file:Day02.org::Sum all repetitions list][Sum all repetitions list]]
sumAllRepetitions_fromList :: [Range] -> Integer
sumAllRepetitions_fromList = sum . map f
  where f = sum . map sumAllRepetitions . decomposeRange
-- Sum all repetitions list ends here

-- [[file:Day02.org::Solution 2][Solution 2]]
solution2 :: IO Integer
solution2 = sumAllRepetitions_fromList <$> readInput
-- Solution 2 ends here
