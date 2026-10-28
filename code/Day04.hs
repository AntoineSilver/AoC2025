{-# LANGUAGE DerivingVia     #-}
{-# LANGUAGE PatternSynonyms #-}
module Day04 where
import           Data.Coerce       (coerce)
import           Data.Massiv.Array (Array, Border (Fill), Comp (Par), DW,
                                    Ix2 (..), Load (makeArray), P, Prim,
                                    Stencil, Sz (Sz), U, compute, makeStencil,
                                    mapStencil)
import qualified Data.Massiv.Array as A
import           Data.Word         (Word8)


-- ANCHOR: cellState
newtype CellState = CellState Word8
  deriving (Eq, Prim) via Word8

instance Show CellState where
  show Empty = "."
  show Full  = "@"

pattern Empty :: CellState
pattern Empty = CellState 0

pattern Full :: CellState
pattern Full = CellState 1

{-# COMPLETE Empty, Full #-}
-- ANCHOR_END: cellState

-- ANCHOR: board
type Board = Array P Ix2 CellState
-- ANCHOR_END: board

-- ANCHOR: readInput
readInput :: IO Board
readInput =
  do
    str <- readFile "./input/Day04.txt"
    let ls = lines str
    let size = Sz (Ix2 (length ls) (length (ls !! 0)))
    let array = makeArray Par size (getCell ls) :: Board
    return array
  where
    getCell ls (Ix2 x y)
      | (ls !! x) !! y == '@' = Full
      | otherwise = Empty
-- ANCHOR_END: readInput

-- ANCHOR: stencils
freeableFromGet :: (Ix2 -> CellState) -> Bool
freeableFromGet get = neighborSum < 4 && center == Full
  where
    center = get (0 :. 0)
    neighborSum = sum $ map (coerce . get) neighbors :: Word8
    neighbors = [-1 :. -1, -1 :. 0, -1 :. 1,
                  0 :. -1,           0 :. 1,
                  1 :. -1,  1 :. 0,  1 :. 1]
{-# INLINE freeableFromGet #-}

freeableStencil :: Stencil Ix2 CellState Bool
freeableStencil = makeStencil (Sz (3 :. 3)) (1 :. 1) freeableFromGet
{-# INLINE freeableStencil #-}

stepStencil :: Stencil Ix2 CellState CellState
stepStencil = makeStencil (Sz (3 :. 3)) (1 :. 1) $ \get ->
  if freeableFromGet get then Empty else get (0 :. 0)
{-# INLINE stepStencil #-}
-- ANCHOR_END: stencils

-- ANCHOR: freeable
countFreeable :: Board -> Integer
countFreeable b = A.foldrS f (0 :: Integer) freeable_manifest
  where f val acc = acc + if val then 1 else 0
        freeable_delayed = mapStencil  (Fill Empty) freeableStencil b
        freeable_manifest = compute freeable_delayed :: Array U Ix2 Bool
-- ANCHOR_END: freeable

-- ANCHOR: solution1
solution1 :: IO Integer
solution1 = countFreeable <$> readInput
-- ANCHOR_END: solution1

-- ANCHOR: allFreeable
countAllFreeable :: Board -> Integer
countAllFreeable b
  | freeable == 0 = 0
  | otherwise     = freeable + countAllFreeable nextBoard
  where
    freeable = countFreeable b
    nextBoard = compute $ mapStencil  (Fill Empty) stepStencil b :: Board
-- ANCHOR_END: allFreeable

-- ANCHOR: solution2
solution2 :: IO Integer
solution2 = countAllFreeable <$> readInput
-- ANCHOR_END: solution2
