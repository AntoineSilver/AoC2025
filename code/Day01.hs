module Day01 where

import           Data.List (foldl')

-- * Types and Helper Functions

-- ANCHOR: types
type Position = Int -- simply alias int
data Rotation = L Int | R Int deriving (Show)
-- ANCHOR_END: types

-- ANCHOR: actions
rotate :: Rotation -> Position -> Position
rotate (L dx) x = x - dx
rotate (R dx) x = x + dx

rotateMod :: Rotation -> Position -> Position
rotateMod r x = rotate r x `mod` 100
-- ANCHOR_END: actions

-- * Input

-- ANCHOR: read
readInput :: IO [Rotation]
readInput = do
  str <- readFile "./input/Day01.txt"
  return $ f <$> lines str
 where
  f :: String -> Rotation
  f ('L' : rs) = L (read rs)
  f ('R' : rs) = R (read rs)
-- ANCHOR_END: read

-- * Part 1

-- ANCHOR: getpos
getPositions :: [Rotation] -> [Position]
getPositions = scanl (flip rotateMod) 50

countZeros :: [Position] -> Int
countZeros = length . filter (== 0)
-- ANCHOR_END: getpos

-- ANCHOR: out1
solution_1 :: IO Int
solution_1 = countZeros . getPositions <$> readInput
-- ANCHOR_END: out1

-- * Part 2

-- ANCHOR: step
step :: Rotation -> (Position, Int) -> (Position, Int)
step r (pos, clicks) = (new_pos_mod, new_clicks)
  where
    new_pos = rotate r pos -- new position with cycles
    new_pos_mod = new_pos `mod` 100 -- without cycles
    new_clicks
      | new_pos >= 100 = new_pos `div` 100
      | new_pos <= 0   = (-new_pos) `div` 100 + if pos == 0 then 0 else 1
      | otherwise      = 0
-- ANCHOR_END: step

-- ANCHOR: countClicks
countClicks :: [Rotation] -> Int
countClicks = snd . foldl' (flip step) (50, 0)
-- ANCHOR_END: countClicks

-- ANCHOR: out2
solution_2 :: IO Int
solution_2 = countClicks <$> readInput
-- ANCHOR_END: out2
