import Criterion.Main
import Day02

-- Day02 Benchmarking
genRange :: Integer -> Range
genRange n = Range (10^n) (10^(n+1) - 1)

inputD2 = [Range 1 9,Range 10 19,Range 32 47,Range 54 79,Range 81 96,Range 101 120,Range 196 268,Range 314 423,Range 472 650,Range 743 999,Range 1000 1030,Range 1744 2691,Range 2799 3721,Range 4653 6357,Range 8200 9999,Range 10000 11903,Range 22946 32222,Range 35704 54213,Range 58933 81008,Range 120068 142180,Range 150748 178674,Range 217886 298699,Range 335082 425865,Range 431725 452205,Range 540949 687222,Range 726684 913526,Range 915593 991111,Range 991404 999999,Range 1000000 1009392,Range 2140045 2264792,Range 3278941 3383621,Range 9084373 9176707,Range 9972438 9999999,Range 10000000 10023331,Range 17039821 17193560,Range 58843645 58909745,Range 62597156 62638027,Range 94888325 95016472,Range 714665437 714803123,Range 3335355312 3335478020,Range 6666577818 6666739950,Range 7575737649 7575766026]

-- The function we're benchmarking.
fib :: Int -> Int
fib m | m < 0     = error "negative!"
      | otherwise = go m
  where
    go 0 = 0
    go 1 = 1
    go n = go (n - 1) + go (n - 2)

-- Our benchmark harness.
main = defaultMain [
  bgroup "Day02-2" [
                 bench (show i)  $ whnf sumAllRepetitions_fromList [genRange i]
                 | i <- [2..200]
               ],
  bench "Day02-2-full" $ whnf sumAllRepetitions_fromList inputD2
  ]
