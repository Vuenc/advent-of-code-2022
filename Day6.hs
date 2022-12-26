module Day6 where

import Data.List

inputLines = fmap lines $ readFile "inputs/input6.txt"

-- Star 1

firstWithoutDuplicates :: (Eq a, Num t) => Int -> [a] -> t -> t
firstWithoutDuplicates n l k
    | length (nub $ take n l) == n = k
    | otherwise = firstWithoutDuplicates n (tail l) (k + 1)


computeSolution1 inputLines = (firstWithoutDuplicates 4 (inputLines !! 0) 0) + 4

-- Star 2


computeSolution2 inputLines = (firstWithoutDuplicates 14 (inputLines !! 0) 0) + 14



-- Solutions (IO)
solution1 = fmap computeSolution1 inputLines
solution2 = fmap computeSolution2 inputLines
