module Day10 where

import Data.List.Split
import Data.List (intercalate)

inputLines = fmap lines $ readFile "inputs/input10.txt"

-- Star 1
registerValues (('a':'d':'d':'x':' ':val):instrs) currentXValue = [currentXValue, newXValue] ++ (registerValues instrs newXValue)
    where newXValue = currentXValue + (read val)
registerValues ("noop":instrs) currentXValue = [currentXValue] ++ (registerValues instrs currentXValue)
registerValues [] _ = []

signalStrengths registerValues = map (\(a,b) -> a*b) $ zip registerValues [1..]

computeSolution1 inputLines = sum [strengths !! (i - 1) | i <- [20, 60, 100, 140, 180, 220]]
    where strengths = signalStrengths $ 1:(registerValues inputLines 1)


-- Star 2

-- crtXPositions = concat $ repeat 

computeSolution2 :: [String] -> [String]
computeSolution2 inputLines = [map pixel $ zip registerValues [0..] | registerValues <- registerValueChunks]
    where registerValueChunks = chunksOf 40 $ 1:(registerValues inputLines 1)
          pixel (registerValue,crtXValue)
            | (abs $ registerValue - crtXValue) <= 1 = '#'
            | otherwise = '.'


-- Solutions (IO)
solution1 = fmap computeSolution1 inputLines
solution2 = do
    input <- inputLines
    let sol2 = computeSolution2 input
    mapM_ putStrLn sol2

ll = lines "addx 15\naddx -11\naddx 6\naddx -3\naddx 5\naddx -1\naddx -8\naddx 13\naddx 4\nnoop\naddx -1\naddx 5\naddx -1\naddx 5\naddx -1\naddx 5\naddx -1\naddx 5\naddx -1\naddx -35\naddx 1\naddx 24\naddx -19\naddx 1\naddx 16\naddx -11\nnoop\nnoop\naddx 21\naddx -15\nnoop\nnoop\naddx -3\naddx 9\naddx 1\naddx -3\naddx 8\naddx 1\naddx 5\nnoop\nnoop\nnoop\nnoop\nnoop\naddx -36\nnoop\naddx 1\naddx 7\nnoop\nnoop\nnoop\naddx 2\naddx 6\nnoop\nnoop\nnoop\nnoop\nnoop\naddx 1\nnoop\nnoop\naddx 7\naddx 1\nnoop\naddx -13\naddx 13\naddx 7\nnoop\naddx 1\naddx -33\nnoop\nnoop\nnoop\naddx 2\nnoop\nnoop\nnoop\naddx 8\nnoop\naddx -1\naddx 2\naddx 1\nnoop\naddx 17\naddx -9\naddx 1\naddx 1\naddx -3\naddx 11\nnoop\nnoop\naddx 1\nnoop\naddx 1\nnoop\nnoop\naddx -13\naddx -19\naddx 1\naddx 3\naddx 26\naddx -30\naddx 12\naddx -1\naddx 3\naddx 1\nnoop\nnoop\nnoop\naddx -9\naddx 18\naddx 1\naddx 2\nnoop\nnoop\naddx 9\nnoop\nnoop\nnoop\naddx -1\naddx 2\naddx -37\naddx 1\naddx 3\nnoop\naddx 15\naddx -21\naddx 22\naddx -6\naddx 1\nnoop\naddx 2\naddx 1\nnoop\naddx -10\nnoop\nnoop\naddx 20\naddx 1\naddx 2\naddx 2\naddx -6\naddx -11\nnoop\nnoop\nnoop"