module Day5 where

import Data.List.Split
import Text.Regex.TDFA

inputLines = fmap lines $ readFile "inputs/input5.txt"

-- Star 1

gridRows inputLines = map gridChunks gridLines
    where gridLines = tail $ reverse $ takeWhile (/="") inputLines
          gridChunks line = map ((\c -> c !! 1))  $ chunksOf 4 (line ++ " ")

gridStacks gridRows = _grid gridRows (replicate (length $ gridRows !! 0) [])
    where _grid (r:rs) stacks = _grid rs (map (\(item, stack) -> (if item /= ' ' then (item:stack) else stack)) $ zip r stacks)
          _grid [] stacks = stacks

parseInstruction :: String -> (Int, Int, Int)
parseInstruction line = (read count, read from - 1, read to - 1) where
    (_, _, _, [count, from, to]) = (line =~ "move ([0-9]+) from ([0-9]+) to ([0-9]+)") :: (String, String, String, [String])

performSingleInstruction from to stacks = map newStack (zip [0..] stacks)
    where
        newStack (i, stack)
            | i == from = tail stack
            | i == to = (head (stacks !! from)):stack
            | otherwise = stack

performInstruction count from to stacks = (iterate (performSingleInstruction from to) stacks)  !! count

performInstructions ((count,from,to):ins) stacks = performInstructions ins (performInstruction count from to stacks)
performInstructions [] stacks = stacks

instructions inputLines = map parseInstruction $ tail $ dropWhile (/="") (inputLines)

computeSolution1 :: [String] -> [Char]
computeSolution1 inputLines = map head finalStacks
    where
        initialGrid = gridStacks $ gridRows inputLines
        finalStacks = performInstructions (instructions inputLines) initialGrid

-- Star 2
performMultiMoveInstruction count from to stacks = map newStack (zip [0..] stacks)
    where
        newStack (i, stack)
            | i == from = drop count stack
            | i == to = (take count (stacks !! from)) ++ stack
            | otherwise = stack

performMultiMoveInstructions ((count,from,to):ins) stacks = performMultiMoveInstructions ins (performMultiMoveInstruction count from to stacks)
performMultiMoveInstructions [] stacks = stacks

computeSolution2 inputLines = map head finalStacks
    where
        initialGrid = gridStacks $ gridRows inputLines
        finalStacks = performMultiMoveInstructions (instructions inputLines) initialGrid



-- Solutions (IO)
solution1 = fmap computeSolution1 inputLines
solution2 = fmap computeSolution2 inputLines
