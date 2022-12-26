module Day17 where

import qualified Data.Set as S
import qualified Data.Map as M
import Data.List (intercalate)
import Debug.Trace (trace)
import qualified Data.Maybe as M
import Data.Maybe (fromMaybe)
import Data.Function (on)

inputLines = fmap lines $ readFile "inputs/input17.txt"

-- Star 1

data Shape = Flat | Cross | Corner | High | Square deriving (Eq, Ord, Show)

data Rock = Rock {
    xy :: (Int, Int), -- The left x position and the bottom y position - y coordinates increase from 0 (floor) upwards
    shape :: Shape
} deriving (Eq, Ord, Show)

x = fst . xy

y = snd . xy

occupiedCoords (Rock {xy=(x,y), shape=Flat}) = [(x,y), (x+1,y), (x+2,y), (x+3,y)]
occupiedCoords (Rock {xy=(x,y), shape=Cross}) = [(x+1,y),(x,y+1),(x+1,y+1),(x+2,y+1),(x+1,y+2)]
occupiedCoords (Rock {xy=(x,y), shape=Corner}) = [(x,y),(x+1,y),(x+2,y),(x+2,y+1),(x+2,y+2)]
occupiedCoords (Rock {xy=(x,y), shape=High}) = [(x,y), (x,y+1), (x,y+2), (x,y+3)]
occupiedCoords (Rock {xy=(x,y), shape=Square}) = [(x,y), (x,y+1), (x+1,y), (x+1,y+1)]

move rock direction rockSet =
    let coordTransform '<' (x,y) = (x-1,y)
        coordTransform '>' (x,y) = (x+1,y)
        coordTransform 'v' (x,y) = (x,y-1)
        movedCoords = map (coordTransform direction) $ occupiedCoords rock
        collides = any (flip S.member $ rockSet) movedCoords
        overstepsBoundary = any (\(x,y) -> y < 0 || x < 0 || x >= 7) movedCoords
        rockSetWithNew = S.union rockSet (S.fromList $ occupiedCoords rock)
        newRock = if (overstepsBoundary || collides) then rock else (rock {xy = coordTransform direction $ xy rock})
        highestYOld = maximum $ map snd $ occupiedCoords rock
    -- in if (overstepsBoundary || collides) && direction == 'v' then (rockSetWithNew, Nothing, highestYOld) else trace (showField rockSet newRock) (rockSet, Just newRock, -1)
    -- in if (overstepsBoundary || collides) && direction == 'v' then (rockSetWithNew, Nothing, highestYOld) else (rockSet, Just newRock, -1)
    in if (overstepsBoundary || collides) && direction == 'v' then (rockSet, Nothing, highestYOld) else (rockSet, Just newRock, -1)

simulateRocks directions (rockSet, Nothing, highestY) (nextShape:nextShapes) = simulateRocks directions (rockSet, Just newRock, highestY) nextShapes
    where newRock = Rock {xy=(2, highestY + 4), shape = nextShape}
simulateRocks (direction:directions) (rockSet, Just rock, highestY) nextShapes = simulateRocks directions (afterFallRockSet, afterFallRock, max afterFallHighestY highestY) nextShapes
    where (_, Just afterJetRock, _) = move rock direction rockSet
          (afterFallRockSet, afterFallRock, afterFallHighestY) = move afterJetRock 'v' rockSet
simulateRocks _ (rockSet, rock, highestY) [] = (rockSet, rock, highestY)

showField rockSet rock =
    let shapeSet = S.fromList $ occupiedCoords rock
        maxY = maximum $ map snd $ S.toList (S.union rockSet shapeSet)
    in intercalate "\n" $ ["|" ++ [if S.member (x,y) shapeSet then '@' else if S.member (x,y) rockSet then '#' else '.' | x <- [0..6]] ++ "|" | y <- reverse $ [0..maxY]] ++ ["+-------+"]

computeSolution inputLine numRocks = highestY + 1
    where (_, _, highestY) = simulateRocks directions (S.empty, Nothing, -1) shapes
          directions = concat $ repeat inputLine
          shapes = take numRocks $ concat $ repeat [Flat, Cross, Corner, High, Square]

computeSolution1 [inputLine] = computeSolution inputLine 2022

-- Star 2

data EfficientState = EfficientState {
    highestY :: [Int], -- for each of the 7 columns, a number relative to the least high column
    rockSet :: S.Set (Int, Int),
    height :: Int,
    insertedRocks :: Int
} deriving (Show)

instance Eq EfficientState where
    (==) = (==) `on` rockSet

instance Ord EfficientState where
    compare = compare `on` rockSet

emptyState = EfficientState {highestY=take 7 $ repeat (-1), rockSet=S.empty, height=0, insertedRocks=0}

insert (EfficientState {highestY=highestY, rockSet=rockSet, height=height, insertedRocks=insertedRocks}) coords =
    let insertedSet = S.union rockSet (S.fromList coords)
        insertedHighestY = foldl (\hy (x,y) -> if hy !! x < y then (take x hy) ++ [y] ++ (drop (x+1) hy) else hy) highestY insertedSet
        newMin = minimum insertedHighestY
        newSet = if newMin > 0 then S.filter ((>=0) . snd) $ S.map (\(x,y) -> (x, y-newMin)) insertedSet else insertedSet
        newHighestY = if newMin > 0 then map (+ (-newMin)) insertedHighestY else insertedHighestY
    -- in EfficientState {highestY=insertedHighestY, rockSet=insertedSet}
    in EfficientState {highestY=newHighestY, rockSet=newSet, height=height + (max newMin 0), insertedRocks=insertedRocks+1}

move' rock direction state =
    let coordTransform '<' (x,y) = (x-1,y)
        coordTransform '>' (x,y) = (x+1,y)
        coordTransform 'v' (x,y) = (x,y-1)
        movedCoords = map (coordTransform direction) $ occupiedCoords rock
        collides = any (flip S.member $ rockSet state) movedCoords
        overstepsBoundary = any (\(x,y) -> y < 0 || x < 0 || x >= 7) movedCoords
        stateNew = insert state $ occupiedCoords rock
        newRock = if (overstepsBoundary || collides) then rock else (rock {xy = coordTransform direction $ xy rock})
        highestYOld = maximum $ map snd $ occupiedCoords rock
    -- in if (overstepsBoundary || collides) && direction == 'v' then (rockSetWithNew, Nothing, highestYOld) else trace (showField rockSet newRock) (rockSet, Just newRock, -1)
    -- in if (overstepsBoundary || collides) && direction == 'v' then (rockSetWithNew, Nothing, highestYOld) else (rockSet, Just newRock, -1)
    -- in if (overstepsBoundary || collides) && direction == 'v' then (stateNew, Nothing, highestYOld) else trace (showField (rockSet state) newRock) (state, Just newRock, -1)
    in if (overstepsBoundary || collides) && direction == 'v' then (stateNew, Nothing) else (state, Just newRock)

-- simulateRocks' directions@((i,_):_) (state, Nothing) (nextShape:nextShapes) periodMap = trace ((show newRock) ++ show state) $ trace (showField (rockSet state) (newRock)) $ simulateRocks' directions (state, Just newRock) nextShapes newPeriodMap
-- simulateRocks' directions@((i,_):_) (state, Nothing) (nextShape:nextShapes) periodMap = trace ((show newRock) ++ show state) $ simulateRocks' directions (state, Just newRock) nextShapes newPeriodMap
-- simulateRocks' directions@((i,_):_) (state, Nothing) (nextShape:nextShapes) periodMap = trace ((show newRock) ++ show state) $ trace (show newPeriodMap) $ simulateRocks' directions (state, Just newRock) nextShapes newPeriodMap

simulateRocks' :: [(Int, Char)] -> (EfficientState, Maybe Rock) -> [Shape] -> Int -> M.Map (Int, Shape) (S.Set EfficientState) -> (EfficientState, Maybe Rock)
simulateRocks' _ (state, rock) _ (-1) periodMap = (state, rock)
simulateRocks' directions@((i,_):_) (state, Nothing) (nextShape:nextShapes) numRocks periodMap = simulateRocks' directions (newState, Just newRock) nextShapes (numRocksAfterPeriodResolve-1) newPeriodMap
    where newRock = Rock {xy=(2, (maximum $ highestY state) + 4 + (length newPeriodMap) * 0), shape = nextShape}
          periodSet = M.fromMaybe (S.empty) $ M.lookup (i,nextShape) periodMap
          periodStateEarly = if S.member state periodSet then S.lookupLE state periodSet else Nothing
          newPeriodMap = M.insert (i,nextShape) (S.insert state periodSet) periodMap
            -- | otherwise = trace ("Period found! " ++ (show (i,nextShape)) ++ (show numRocks)) periodMap
          (newState, numRocksAfterPeriodResolve) = case periodStateEarly of
            Nothing -> (state, numRocks)
            Just earlyState -> resolvePeriod earlyState state numRocks
simulateRocks' ((i,direction):directions) (state, Just rock) nextShapes numRocks periodMap = simulateRocks' directions (afterFallState, afterFallRock) nextShapes numRocks periodMap
    where (_, Just afterJetRock) = move' rock direction state
          (afterFallState, afterFallRock) = move' afterJetRock 'v' state
-- simulateRocks' _ (state, rock) [] _ periodMap = (state, rock)

resolvePeriod :: EfficientState -> EfficientState -> Int -> (EfficientState, Int)
resolvePeriod periodStateEarly periodStateLate numRocks =
    let periodLength = (insertedRocks periodStateLate - insertedRocks periodStateEarly)
        heightDifference = (height periodStateLate - height periodStateEarly)
        numRepetitions = numRocks `div` periodLength
        numRocksAfterRepetition = numRocks - numRepetitions * periodLength
        stateAfterRepetition = periodStateLate {height=height periodStateLate + numRepetitions*heightDifference, insertedRocks=insertedRocks periodStateLate + numRepetitions}
    in (stateAfterRepetition, numRocksAfterRepetition) 

-- computeSolution' inputLine numRocks = height finalState + (maximum $ highestY finalState) + 1
computeSolution' inputLine numRocks = trace (showResult (finalState, Nothing) ++ "\n" ++ (show finalState)) $ height finalState + (maximum $ highestY finalState) + 1
    where (finalState, _) = simulateRocks' directions (emptyState, Nothing) shapes numRocks (M.empty)
          directions = concat $ repeat (zip [0..] inputLine)
          shapes = concat $ repeat [Flat, Cross, Corner, High, Square]

-- computeSolution2 [inputLine] = computeSolution inputLine 1000000000000


showResult (state,rock) = showField (rockSet state) (fromMaybe (Rock {xy=(10,10), shape=Flat}) rock)
printResult = putStrLn . showResult

computeSolution2 [inputLine] = computeSolution' inputLine 1000000000000


-- Solutions (IO)
solution1 = fmap computeSolution1 inputLines
solution2 = fmap computeSolution2 inputLines
