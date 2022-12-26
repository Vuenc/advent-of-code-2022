module Day4 where

import Data.List (elemIndex)

inputLines = fmap lines $ readFile "inputs/input4.txt"

-- Star 1
splitRange :: String -> ((Integer, Integer), (Integer, Integer))
splitRange line = ((read a, read b), (read c, read d))
    where
        (a, _b:b) = splitAt (maybe (-1) id $ elemIndex '-' part1) part1
        (c, _d:d) = splitAt (maybe (-1) id $ elemIndex '-' part2) part2
        (part1, _part2:part2) = splitAt (maybe (-1) id $ elemIndex ',' line) line

isContained ((a,b), (c,d)) = ((c <= a) && (b <= d)) || (a <= c) && (d <= b)

computeSolution1 inputLines = sum [if isContained $ splitRange line then 1 else 0 | line <- inputLines]


-- Star 2

overlaps ((a,b), (c,d)) = not (b < c || d < a)

computeSolution2 inputLines = sum [if overlaps $ splitRange line then 1 else 0 | line <- inputLines]


-- Solutions (IO)
solution1 = fmap computeSolution1 inputLines
solution2 = fmap computeSolution2 inputLines
