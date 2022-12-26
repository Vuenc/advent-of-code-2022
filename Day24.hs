module Day24 where

import qualified Data.Map as M
import Data.Function (on)
import Algorithm.Search (dijkstra)
import Data.Maybe (fromMaybe)
import Data.List (intercalate)

inputLines = fmap lines $ readFile "inputs/input24.txt"

-- Star 1

data BlizzardState = BlizzardState {
    blizzards :: M.Map (Int, Int) [Char],
    height :: Int,
    width :: Int,
    time :: Int
}

instance Eq BlizzardState where
    (==) = (==) `on` time

instance Ord BlizzardState where
    compare = compare `on` time

instance Show BlizzardState where
    show BlizzardState{blizzards=blizzards,width=width,height=height} = "\n" ++ intercalate "\n" [[toChar $ M.lookup (x,y) blizzards | x <- [0..width-1]] | y <- [0..height-1]]
        where toChar Nothing = '.'
              toChar (Just [c]) = c
              toChar (Just l) = if length l < 10 then (show (length l) !! 0) else '+'

blizzardState inputLines = 
    let height = length inputLines - 2
        width = length (inputLines !! 0) - 2
        relevantLines = take height $ drop 1 inputLines
        blizzardSymbols = ['>','<','v','^']
        blizzards = M.fromList [((x,y), [c]) | (y,line) <- (zip [0..] relevantLines), (x,c) <- (zip [0..] (take width $ drop 1 line)), c `elem` blizzardSymbols]
    in BlizzardState {blizzards=blizzards, height=height, width=width, time=0}

moveBlizzard BlizzardState{width=width} ((x,y), '>') = (if x < width-1 then (x+1,y) else (0,y), '>')
moveBlizzard BlizzardState{width=width} ((x,y), '<') = (if x > 0 then (x-1,y) else (width-1,y), '<')
moveBlizzard BlizzardState{height=height} ((x,y), 'v') = (if y < height-1 then (x,y+1) else (x,0), 'v')
moveBlizzard BlizzardState{height=height} ((x,y), '^') = (if y > 0 then (x,y-1) else (x,height-1), '^')

evolveState state@BlizzardState{blizzards=blizzards, time=time} = state {blizzards=newBlizzards, time=time+1}
    where newBlizzardsList = map (moveBlizzard state) $ concat $ map (\((x,y),l) -> [((x,y),c) | c <- l]) $ M.toList blizzards
          newBlizzards = foldl (\m (key, char) -> M.insert key (char:(fromMaybe [] (M.lookup key m))) m) M.empty newBlizzardsList

isValidCoord width height (x,y) = (x >= 0 && x < width && y >= 0 && y < height) || (x,y) == (0,-1) || (x,y) == (width-1, height)

computeSolution1 inputLines =
    let initialState@BlizzardState{width=width, height=height} = blizzardState inputLines
        blizzardList = (iterate evolveState) initialState
        neighbors ((partyX, partyY), blizzardState) = 
            let potentialNeighborCoords = [(x,y) | (x,y) <- [(partyX+1,partyY), (partyX-1,partyY), (partyX,partyY+1), (partyX,partyY-1), (partyX,partyY)], isValidCoord width height (x,y)]
                nextBlizzard = blizzardList !! (time blizzardState + 1)
                neighborCoords = filter (flip M.notMember $ blizzards nextBlizzard) potentialNeighborCoords
            in [((x,y), nextBlizzard) | (x,y) <- neighborCoords]
        -- Just startCell = find ((=='S') . char) $ concat field
        startCell = ((0,-1), initialState)
        transitionCost _ _ = 1
        solutionFound (coords, _) = coords == (width-1, height)
        Just (cost, solution) = dijkstra neighbors transitionCost solutionFound startCell
    in cost


-- Star 2

computeSolution2 inputLines =
    let initialState@BlizzardState{width=width, height=height} = blizzardState inputLines
        blizzardList = (iterate evolveState) initialState
        newTrip oldTrip partyXY
            | partyXY == (width-1, height) && oldTrip == 0 = 1
            | partyXY == (0, -1) && oldTrip == 1 = 2
            | otherwise = oldTrip
        neighbors ((partyX, partyY), blizzardState, trip) =
            let potentialNeighborCoords = [(x,y) | (x,y) <- [(partyX+1,partyY), (partyX-1,partyY), (partyX,partyY+1), (partyX,partyY-1), (partyX,partyY)], isValidCoord width height (x,y)]
                nextBlizzard = blizzardList !! (time blizzardState + 1)
                neighborCoords = filter (flip M.notMember $ blizzards nextBlizzard) potentialNeighborCoords
            in [((x,y), nextBlizzard, newTrip trip (partyX, partyY)) | (x,y) <- neighborCoords]
        -- Just startCell = find ((=='S') . char) $ concat field
        startCell = ((0,-1), initialState, 0)
        transitionCost _ _ = 1
        solutionFound (coords, _, trip) = coords == (width-1, height) && trip == 2
        Just (cost, solution) = dijkstra neighbors transitionCost solutionFound startCell
    in cost

l = lines "#.######\n#>>.<^<#\n#.<..<<#\n#>v.><>#\n#<^v^^>#\n######.#"

-- Solutions (IO)
solution1 = fmap computeSolution1 inputLines
solution2 = fmap computeSolution2 inputLines
