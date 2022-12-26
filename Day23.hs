module Day23 where

import qualified Data.Map as M
import Data.IntMap (insert)
import Debug.Trace (traceShow)
import Data.List (intercalate, findIndex)

inputLines = fmap lines $ readFile "inputs/input23.txt"

data FieldContents = Elf | FutureElf (Int, Int) | Blocked deriving (Eq, Show)

type ElfMap = M.Map (Int, Int) FieldContents

showMap :: ElfMap -> IO ()
showMap elfMap = putStrLn $ intercalate "\n" [[if M.member (x,y) elfMap then '#' else '.' | x <- [minX..maxX]] | y <- [minY..maxY]]
    where (xs, ys) = (map fst $ M.keys elfMap, map snd $ M.keys elfMap)
          (minX, maxX, minY, maxY) = (min 0 $ minimum xs, maximum xs, min 0 $ minimum ys, maximum ys)

-- Star 1

buildElfMap :: [[Char]] -> ElfMap
buildElfMap inputLines = M.fromList $ zip coords $ repeat Elf
 where coords = [(x,y) | (y, row) <- zip [0..] inputLines, (x,c) <- zip [0..] row, c == '#']

adjacentCoords x y = [(x+dx,y+dy) | dx <- [-1..1], dy <- [-1..1], (dx, dy) /= (0, 0)]


planMove :: [Char] -> (Int, Int) -> ElfMap -> ElfMap
planMove (lookingDirection:restDirections) (x,y) elfMap =
    let directions 'N' = [(x-1,y-1),(x,y-1),(x+1,y-1)]
        directions 'S' = [(x-1,y+1),(x,y+1),(x+1,y+1)]
        directions 'W' = [(x-1,y-1),(x-1,y),(x-1,y+1)]
        directions 'E' = [(x+1,y-1),(x+1,y),(x+1,y+1)]
        directionCoords = directions lookingDirection
        cont@[cont1,cont2,cont3] = map (flip M.lookup $ elfMap) directionCoords
    in if any ((==Just Elf) . (flip M.lookup $ elfMap)) $ adjacentCoords x y
       then if all (/=Just Elf) cont
            -- then traceShow ((show (x,y)) ++ " -> " ++ [lookingDirection] ++ " (" ++ (show cont) ++ ")") $ planStep (x,y) (directionCoords !! 1) cont2 elfMap
            then planStep (x,y) (directionCoords !! 1) cont2 elfMap
            else planMove restDirections (x,y) elfMap
    --    else traceShow ((show (x,y)) ++ " does not need to move") elfMap
       else elfMap
-- planMove [] (x,y) elfMap = traceShow ((show (x,y)) ++ " cannot move") elfMap
planMove [] (x,y) elfMap = elfMap

planStep oldXY newXY (Just (FutureElf otherOldXY)) elfMap = M.insert otherOldXY Elf $ M.insert newXY Blocked elfMap
planStep oldXY newXY (Just Blocked) elfMap = elfMap
planStep oldXY newXY (Nothing) elfMap = M.insert newXY (FutureElf oldXY) elfMap

executeMove (xy, Elf) elfMap = elfMap
executeMove (xy, FutureElf oldXY) elfMap = M.insert xy Elf $ M.delete oldXY elfMap
executeMove (xy, Blocked) elfMap = M.delete xy elfMap

simulateRound (directions, elfMap) = 
    let fieldAfterPlan = foldr (planMove directions) elfMap (M.keys elfMap)
        fieldAfterExecution = foldr executeMove elfMap (M.toList fieldAfterPlan)
    -- in (tail (traceShow directions directions) ++ [head directions], fieldAfterExecution)
    in (tail directions ++ [head directions], fieldAfterExecution)

simulateNRounds elfMap n = finalMap
    where (_, finalMap) = (iterate simulateRound ("NSWE", elfMap)) !! n

computeSolution1 inputLines =
    let initialMap = buildElfMap inputLines
        finalMap = simulateNRounds initialMap 10
        (xs, ys) = (map fst $ M.keys finalMap, map snd $ M.keys finalMap)
        (minX, maxX, minY, maxY) = (minimum xs, maximum xs, minimum ys, maximum ys)
    in ((maxX - minX + 1) * (maxY - minY + 1)) - length finalMap

-- Star 2

computeSolution2 inputLines = 
    let initialMap = buildElfMap inputLines
        mapList = (iterate simulateRound ("NSWE", initialMap))
        Just i = findIndex (\((_, map1), (_, map2)) -> map1 == map2) (zip mapList (tail mapList))
    in i + 1


-- Solutions (IO)
solution1 = fmap computeSolution1 inputLines
solution2 = fmap computeSolution2 inputLines

l = lines ".....\n..##.\n..#..\n.....\n..##.\n.....\n"
l2 = lines "..............\n..............\n.......#......\n.....###.#....\n...#...#.#....\n....#...##....\n...#.###......\n...##.#.##....\n....#..#......\n..............\n..............\n..............\n"

m = buildElfMap l
m2 = buildElfMap l2
