-- [[file:Day01.org::Setup][Setup]]
import Data.List (foldl')
-- Setup ends here

-- [[file:Day01.org::Data structure for rotations][Data structure for rotations]]
data Rotation = L Int | R Int deriving (Show)
-- Data structure for rotations ends here

-- [[file:Day01.org::Rotations as Actions][Rotations as Actions]]
rotate (L dx) x = x - dx
rotate (R dx) x = x + dx

rotateMod (L dx) x = (x - dx) `mod` 100
rotateMod (R dx) x = (x + dx) `mod` 100
-- Rotations as Actions ends here

-- [[file:Day01.org::Read Input][Read Input]]
readInput :: IO [Rotation]
readInput = do
    str <- readFile "../input/Day01.txt"
    return $ f <$> lines str
  where
    f :: String -> Rotation
    f ('L' : rs) = L (read rs)
    f ('R' : rs) = R (read rs)
-- Read Input ends here

-- [[file:Day01.org::Dial Positions][Dial Positions]]
getPositions :: [Rotation] -> [Int]
getPositions = foldl' f [50]
  where
    f :: [Int] -> Rotation -> [Int]
    f pos rot = pos ++ [rotateMod rot $ last pos]
-- Dial Positions ends here

-- [[file:Day01.org::Count Zeros][Count Zeros]]
countZeros :: [Int] -> Int
countZeros = length . filter (== 0)
-- Count Zeros ends here

-- [[file:Day01.org::Solution 1][Solution 1]]
solution_1 :: IO Int
solution_1 = countZeros . getPositions <$> readInput
-- Solution 1 ends here

-- [[file:Day01.org::New Count Zeros][New Count Zeros]]
countClicks :: [Rotation] -> Int
countClicks rs = snd $ foldl' f (50, 0) rs
  where
    f :: (Int, Int) -> Rotation -> (Int, Int)
    f (pos, clicks) r = (newpos `mod` 100, clicks + add_clicks)
      where
        newpos = rotate r pos
        add_clicks
            | newpos >= 100 = newpos `div` 100
            | newpos <= 0 = (-newpos) `div` 100 + if pos == 0 then 0 else 1
            | otherwise = 0
-- New Count Zeros ends here

-- [[file:Day01.org::Solution 1][Solution 1]]
solution_2 :: IO Int
solution_2 = countClicks <$> readInput
-- Solution 1 ends here
