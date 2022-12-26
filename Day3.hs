module Day3 where

import Data.List (intersect)
import Data.Char (ord, isLower, toLower)
import Data.Sequence (fromList, chunksOf)

inputLines = fmap lines $ readFile "inputs/input3.txt"

-- Star 1
compartments contents = (take (len `div` 2) contents, drop (len `div` 2) contents)
    where len = length contents

commonItem (c1, c2) = head $ intersect c1 c2

itemValue item
    | Data.Char.isLower item = Data.Char.ord item - Data.Char.ord 'a' + 1
    | otherwise              = Data.Char.ord (Data.Char.toLower item) - Data.Char.ord 'a' + 27


computeSolution1 inputLines = sum (map (itemValue . commonItem . compartments) inputLines)


-- Star 2
commonItem3 [c1,c2,c3] = head $ (intersect c1 (intersect c2 c3))

chunksOf3 (x1:x2:x3:xs) = [[x1,x2,x3]] ++ chunksOf3 xs
chunksOf3 [] = []
chunksOf3 l = [l]

computeSolution2 inputLines = sum (map (itemValue . commonItem3) inputChunks)
    where inputChunks = chunksOf3 inputLines

-- Solutions (IO)
solution1 = fmap computeSolution1 inputLines
solution2 = fmap computeSolution2 inputLines
