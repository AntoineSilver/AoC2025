module Day04 where

-- [[file:Day04.org::Parse Data][Parse Data]]
testInput :: [[Integer]]
testInput =
    [ [0, 0, 1, 1, 0, 1, 1, 1, 1, 0] -- ..@@.@@@@.
    , [1, 1, 1, 0, 1, 0, 1, 0, 1, 1] -- @@@.@.@.@@
    , [1, 1, 1, 1, 1, 0, 1, 0, 1, 1] -- @@@@@.@.@@
    , [1, 0, 1, 1, 1, 1, 0, 0, 1, 0] -- @.@@@@..@.
    , [1, 1, 0, 1, 1, 1, 1, 0, 1, 1] -- @@.@@@@.@@
    , [0, 1, 1, 1, 1, 1, 1, 1, 0, 1] -- .@@@@@@@.@
    , [0, 1, 0, 1, 0, 1, 0, 1, 1, 1] -- .@.@.@.@@@
    , [1, 0, 1, 1, 1, 0, 1, 1, 1, 1] -- @.@@@.@@@@
    , [0, 1, 1, 1, 1, 1, 1, 1, 1, 0] -- .@@@@@@@@.
    , [1, 0, 1, 0, 1, 1, 1, 0, 1, 0] -- @.@.@@@.@.
    ]

readInput :: IO [[Integer]]
readInput = do
    str <- readFile "./input/Day04.txt"
    return $ f <$> lines str
  where
    f :: String -> [Integer]
    f = map (\c -> if c == '@' then 1 else 0)
-- Parse Data ends here

-- [[file:Day04.org::Shift Grid][Shift Grid]]
shiftU :: [[Integer]] -> [[Integer]]
shiftU g = tail g ++ [take w (repeat 0)]
  where
    w = length (head g)

shiftD :: [[Integer]] -> [[Integer]]
shiftD g = take w (repeat 0) : init g
  where
    w = length (head g)

shiftL :: [[Integer]] -> [[Integer]]
shiftL = map (\r -> tail r ++ [0])

shiftR :: [[Integer]] -> [[Integer]]
shiftR = map (\r -> 0 : init r)
-- Shift Grid ends here

-- [[file:Day04.org::Solution 1][Solution 1]]
mapFree :: [[Integer]] -> [[Integer]]
mapFree g = freeMap
              where nCount = foldr1 (zipWith (zipWith (+))) [u,d,l,r,ul,ur,dl,dr]
                    freeMapFull = map (map (<4)) nCount
                    freeMap = zipWith (zipWith (\f -> \x -> if f then x else 0)) freeMapFull g
                    u = shiftU g
                    d = shiftD g
                    l = shiftL g
                    r = shiftR g
                    ul = shiftL u
                    ur = shiftR u
                    dl = shiftL d
                    dr = shiftR d

countFree :: [[Integer]] -> Integer
countFree g = sum $ map sum (mapFree g)

solution1 = countFree <$> readInput
-- Solution 1 ends here

-- [[file:Day04.org::Solution 2][Solution 2]]
countAllFree g
             | nbFree == 0 = 0
             | otherwise = nbFree + countAllFree newMap
             where freeMap = mapFree g
                   nbFree = sum $ map sum freeMap
                   newMap = zipWith (zipWith (-)) g freeMap

solution2 = countAllFree <$> readInput
-- Solution 2 ends here
