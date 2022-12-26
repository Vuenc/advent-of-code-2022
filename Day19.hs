module Day19 where

import Text.Regex.TDFA
import Algorithm.Search (dijkstra)
import Data.Function (on)
import qualified Data.Set as S
import qualified Data.Map as M

import qualified Debug.Trace
import Data.List (intercalate, sortOn, sort)
reallyTrace = Debug.Trace.trace
-- trace = Debug.Trace.trace
trace a b = b

inputLines = fmap lines $ readFile "inputs/input19.txt"

-- Star 1

data Resources = Res {
    ore :: Int,
    clay :: Int,
    obsidian :: Int,
    geodes :: Int,
    oreRobots :: Int,
    clayRobots :: Int,
    obsidianRobots :: Int,
    geodeRobots :: Int
} deriving (Eq)

emptyResources = Res{ore=0, clay=0, obsidian=0, geodes=0, oreRobots=0, clayRobots=0, obsidianRobots=0, geodeRobots=0}

instance Ord Resources where
    compare = compare `on` valueTuple

instance Show Resources where
    show Res {ore=ore, clay=clay, obsidian=obsidian, geodes=geodes, oreRobots=oreRobots, clayRobots=clayRobots, obsidianRobots=obsidianRobots, geodeRobots=geodeRobots
        } = "(" ++ show ore ++ "," ++ show clay ++ "," ++ show obsidian ++ "," ++ show geodes ++ "|" ++ show oreRobots ++ "," ++ show clayRobots ++ "," ++ show obsidianRobots ++ "," ++ show geodeRobots ++ ")"

--  trace (show timeLeft ++ " " ++ show (ore resources, clay resources, obsidian resources, geodes resources, oreRobots resources, clayRobots resources, obsidianRobots resources, geodeRobots resources))

data State = State {
    resources :: Resources,
    timeLeft :: Int
}

instance Show State where
    show State {resources=resources, timeLeft=timeLeft} = (show timeLeft) ++ " - " ++ (show resources)

instance Eq State where
    (==) = (==) `on` resources

instance Ord State where
    compare = compare `on` resources


parseBlueprint :: String -> Blueprint
parseBlueprint line =
    let (_, _, _, nums) = line =~ "Each ore robot costs ([0-9]*) ore. Each clay robot costs ([0-9]*) ore. Each obsidian robot costs ([0-9]*) ore and ([0-9]*) clay. Each geode robot costs ([0-9]*) ore and ([0-9]*) obsidian." :: (String, String, String, [String])
        [oreRobotOre, clayRobotOre, obsRobotOre, obsRobotClay, geodeRobotOre, geodeRobotObsidian] = map read nums
    in Blueprint{oreRobot=emptyResources{ore=(-oreRobotOre), oreRobots=1}, clayRobot=emptyResources{ore=(-clayRobotOre), clayRobots=1},
        obsidianRobot=emptyResources{ore=(-obsRobotOre), clay=(-obsRobotClay), obsidianRobots=1}, geodeRobot=emptyResources{ore=(-geodeRobotOre), obsidian=(-geodeRobotObsidian), geodeRobots=1}}

data Blueprint = Blueprint {
    oreRobot :: Resources,
    clayRobot :: Resources,
    obsidianRobot :: Resources,
    geodeRobot :: Resources
} deriving (Show)

possibleRepeatsOfOperation resources operationResources =
    minimum [res resources `div` res operationResources | res <- [ore, clay, obsidian, geodes, oreRobots, clayRobots, obsidianRobots, geodeRobots], res operationResources > 0]

sufficientResources resources operationResources =
    all id [res resources >= (-res operationResources) | res <- [ore, clay, obsidian], res operationResources /= 0]

robotsWork oldResources@Res{oreRobots=oreRobots, clayRobots=clayRobots, obsidianRobots=obsidianRobots, geodeRobots=geodeRobots} afterOperationResources@Res{ore=ore, clay=clay, obsidian=obsidian, geodes=geodes} =
    afterOperationResources{ore=ore+oreRobots, clay=clay+clayRobots, obsidian=obsidian+obsidianRobots, geodes=geodes+geodeRobots}

performOperation resources@Res{ore=ore, clay=clay, obsidian=obsidian, geodes=geodes, oreRobots=oreRobots, clayRobots=clayRobots, obsidianRobots=obsidianRobots, geodeRobots=geodeRobots
        } operationResources@Res{ore=resOre, clay=resClay, obsidian=resObsidian, geodes=resGeodes, oreRobots=resOreRobots, clayRobots=resClayRobots, obsidianRobots=resObsidianRobots, geodeRobots=resGeodeRobots} =
    resources{ore=ore+resOre, clay=clay+resClay, obsidian=obsidian+resObsidian, geodes=geodes+resGeodes, oreRobots=oreRobots+resOreRobots,
        clayRobots=clayRobots+resClayRobots, obsidianRobots=obsidianRobots+resObsidianRobots, geodeRobots=geodeRobots+resGeodeRobots}

-- nextStates Blueprint{oreRobot=buildOreRobot, clayRobot=buildClayRobot, obsidianRobot=buildObsidianRobot, geodeRobot=buildGeodeRobot} state@(State {resources=resources, timeLeft=timeLeft}) = 
--     let afterWorkResources = robotsWork resources
--         potentialOperations = filter (sufficientResources afterWorkResources)  [buildOreRobot, buildClayRobot, buildObsidianRobot, buildGeodeRobot]
--         resourcesAfterOperations = map (performOperation afterWorkResources) potentialOperations
--     in trace (show state) [State{resources=res, timeLeft=timeLeft - 1} | res <- take 4 $ resourcesAfterOperations ++ [afterWorkResources]] -- "take 4": if sufficient resources for all robots are available, waiting is NOT an option

nextStates Blueprint{oreRobot=buildOreRobot, clayRobot=buildClayRobot, obsidianRobot=buildObsidianRobot, geodeRobot=buildGeodeRobot} resources = 
    let
        -- afterWorkResources = robotsWork resources
        needMoreOreRobots = oreRobots resources < -(maximum $ map ore [buildClayRobot, buildObsidianRobot, buildGeodeRobot])
        needMoreClayRobots = clayRobots resources < -clay buildObsidianRobot
        needMoreObsidianRobots = obsidianRobots resources < -obsidian buildGeodeRobot
        neededOperations = [operation | (needed, operation) <- [(needMoreOreRobots, buildOreRobot), (needMoreClayRobots, buildClayRobot), (needMoreObsidianRobots, buildObsidianRobot), (True, buildGeodeRobot)]]
        potentialOperations = filter (sufficientResources resources)  neededOperations
        resourcesAfterOperations = map (performOperation resources) potentialOperations
        resourcesAfterWork = map (robotsWork resources) resourcesAfterOperations 
        result = resourcesAfterWork ++ (if length potentialOperations < length neededOperations then [robotsWork resources resources] else []) -- if sufficient resources for all robots are available, waiting is NOT an option
    -- in trace (show result) result
    in result


    -- I first thought I needed recursion for this, but turns out you can only build 1 robot/minute

        -- potentialOperations = [(op, times) | op <- [buildOreRobot, buildClayRobot, buildObsidianRobot, buildGeodeRobot], let times = possibleRepeatsOfOperation afterWorkResources op, times > 0]
    --     _nextStates _resources =
    --         let potentialOperations = filter (sufficientResources _resources)  [buildOreRobot, buildClayRobot, buildObsidianRobot, buildGeodeRobot]
    --             resourcesAfterOperations = map (performOperation _resources) potentialOperations
    --         in (_resources:(concat $ map _nextStates resourcesAfterOperations))
    -- in _nextStates afterWorkResources

nextTimestepStates :: Blueprint -> S.Set Resources -> S.Set Resources -> S.Set Resources
nextTimestepStates blueprint seenStates currentStates = 
    (S.\\) (S.fromList $ concat $ map (nextStates blueprint) $ S.toList currentStates) seenStates 

computeMostFinalGeodesUpperBound timeLeft Res{geodes=geodes, geodeRobots=geodeRobots} = geodes + geodeRobots * timeLeft


bfs :: Blueprint -> S.Set Resources -> S.Set Resources -> Int -> Int
bfs _ _ currentStates 0 =  trace (show currentStates) $ maximum $ S.map geodes currentStates
bfs blueprint seenStates currentStates timeLeft =
    let newStates = nextTimestepStates blueprint seenStates currentStates
        mostFinalGeodesUpperBound = maximum $ S.map (computeMostFinalGeodesUpperBound timeLeft) newStates
        relevantStates = S.filter (couldStillProduceEnoughGeodes blueprint timeLeft (mostFinalGeodesUpperBound)) newStates
        -- paretoBoundaryStates = paretoBoundaryWithOther valueTuple (S.toList relevantStates) (S.toList seenStates)
        paretoBoundaryStates = paretoBoundaryWithOtherSet valueTuple relevantStates seenStates
        newSeenStates = S.union seenStates relevantStates
    in trace (((intercalate "\n" $ map show $ zip (repeat (timeLeft-1)) paretoBoundaryStates)) ++ "\n\n") $ bfs blueprint newSeenStates (S.fromList paretoBoundaryStates) (timeLeft - 1)

valueTuple Res{ore=ore, clay=clay, obsidian=obsidian, geodes=geodes, oreRobots=oreRobots, clayRobots=clayRobots, obsidianRobots=obsidianRobots, geodeRobots=geodeRobots
    } = [-geodeRobots, -geodes, -obsidianRobots, -obsidian, -clayRobots, -clay, -oreRobots, -ore]

-- dfs :: Blueprint -> M.Map Resources Int -> Resources -> Int -> Int -> (M.Map Resources Int, Int)
-- dfs _ seenStates currentState 0 _ = trace ("*" ++ (show $ geodes currentState) ++ " - seen: " ++ (show $ length seenStates)) (seenStates, geodes currentState)
-- dfs blueprint seenStates currentState timeLeft mostGeodesFound =
--     -- delete those states which have been seen before at this depth or higher (but NOT if seen only at lower depths)
--     let newStates = M.differenceWith (\new seen -> if new >= seen then Just new else Nothing) (M.fromList $ zip (nextStates blueprint currentState) (repeat timeLeft) ) seenStates
--         --  newStates = (S.\\) (S.fromList $ nextStates blueprint currentState) seenStates --nextTimestepStates blueprint seenStates currentStates
--         relevantStates = M.filterWithKey (\state _ -> couldStillProduceEnoughGeodes blueprint timeLeft mostGeodesFound state) newStates
--         newSeenStates = M.unionWith max seenStates relevantStates
--         -- relevantStatesSorted = sortOn valueTuple $ map fst $ M.toList relevantStates
--         paretoBoundaryStatesSorted = paretoBoundaryWith valueTuple $ map fst $ M.toList relevantStates        
--         iterateChildren (_seenStates, _mostGeodesFound) nextState =
--             let (childSeenStates, childMostGeodesFound) = dfs blueprint _seenStates nextState (timeLeft - 1) _mostGeodesFound
--                 _newSeenStates =  M.unionWith max _seenStates childSeenStates
--             in trace (show timeLeft ++ show nextState) (_newSeenStates, max childMostGeodesFound _mostGeodesFound)
--     in foldl iterateChildren (newSeenStates, mostGeodesFound) paretoBoundaryStatesSorted
--     -- in trace (((intercalate "\n" $ map show $ zip (repeat (timeLeft-1)) $ S.toList relevantStates)) ++ "\n\n") $ bfs blueprint newSeenStates relevantStates (timeLeft - 1)
        
paretoBoundary :: Ord a => [[a]] -> [[a]]
paretoBoundary l@(_:_) = _paretoBoundary x xs
    where (x:xs) = sort l
          _paretoBoundary current (l:ls)
            | all (\(a,b) -> a <= b) $ zip current l = _paretoBoundary current ls
            | otherwise = current:(_paretoBoundary l ls)
          _paretoBoundary current [] = [current]
paretoBoundary [] = []

paretoBoundaryWith :: Ord a => (b -> [a]) -> [b] -> [b]
paretoBoundaryWith f l@(_:_) = _paretoBoundary x xs
    where (x:xs) = sortOn f l
          _paretoBoundary current (l:ls)
            | all (\(a,b) -> a <= b) $ zip (f current) (f l) = _paretoBoundary current ls
            | otherwise = current:(_paretoBoundary l ls)
          _paretoBoundary current [] = [current]
paretoBoundaryWith _ [] = []

paretoBoundaryWithOther :: Ord a => (b -> [a]) -> [b] -> [b] -> [b]
paretoBoundaryWithOther f l@(_:_) other = _paretoBoundary x xs
    where (x:xs) = sortOn (f . snd) ((zip (repeat True) l) ++ (zip (repeat False) other))
          _paretoBoundary current@(keepCurrent,currentVal) (l:ls)
            | all (\(a,b) -> a <= b) $ zip (f $ currentVal) (f $ snd l) = _paretoBoundary current ls
            | keepCurrent = (currentVal):(_paretoBoundary l ls)
            | not keepCurrent = _paretoBoundary l ls
          _paretoBoundary (True,currentVal) [] = [currentVal]
          _paretoBoundary (False,currentVal) [] = []
paretoBoundaryWithOther _ [] _ = []

paretoBoundaryWithOtherSet :: (Ord a, Ord b, Eq b) => (b -> [a]) -> (S.Set b) -> (S.Set b) -> [b]
paretoBoundaryWithOtherSet f set setOther
    | S.null set = []
    | otherwise  = _paretoBoundary x xs
        where (x:xs) = S.toList $ (S.union (S.map (\x -> (x,True)) set) (S.map (\x -> (x,False)) setOther))
        -- ((zip (repeat True) l) ++ (zip (repeat False) other))
            --   _paretoBoundary :: (b, Bool) -> [(b, Bool)] -> [b]
              _paretoBoundary current@(currentVal,keepCurrent) (l@(lval, _):ls)
                  | all (\(a,b) -> a <= b) $ zip (f currentVal) (f lval) = _paretoBoundary current ls
                  | keepCurrent = currentVal:(_paretoBoundary l ls)
                  | not keepCurrent = _paretoBoundary l ls
              _paretoBoundary (currentVal,True) [] = [currentVal]
              _paretoBoundary (currentVal,False) [] = [currentVal]


couldStillProduceEnoughGeodes (Blueprint {geodeRobot=Res {obsidian=geodeRobotObsidianCost}
        }) timeLeft mostFinalGeodesUpperBound (Res {obsidian=obsidian, obsidianRobots=obsidianRobots, geodeRobots=geodeRobots, geodes=geodes}) =
    -- | geodes > 0 || geodeRobots > 0 = True
    -- | otherwise =
    let maxPossibleObsidian = obsidian + obsidianRobots*timeLeft + ((timeLeft - 1) * timeLeft) `div` 2
        maxPossibleNewGeodeRobots = min timeLeft $ maxPossibleObsidian `div` (-geodeRobotObsidianCost)
        maxGeodes = geodes + geodeRobots * timeLeft + ((maxPossibleNewGeodeRobots-1) * maxPossibleNewGeodeRobots) `div` 2
        -- canAffordGeodeRobot = maxPossibleObsidian > (-geodeRobotObsidianCost)
        -- canAffordGeodeRobotFrom = 
    in maxGeodes >= mostFinalGeodesUpperBound 

-- computeSolutionDijkstra blueprint =
--     let maxGeodes = 12*25
--         numSteps = 22
--         startState = State{resources=emptyResources{oreRobots=1}, timeLeft=numSteps}
--         transitionCost _ State{resources=Res{geodes=geodes}, timeLeft=0} = 1 + maxGeodes -  geodes
--         transitionCost _ _ = 1
--         solutionFound State{timeLeft=timeLeft} = timeLeft == 0
--         Just (cost, solution) = dijkstra (nextStates blueprint) transitionCost solutionFound startState
--     in maxGeodes - (cost  - numSteps)

-- computeSolution blueprint numSteps = dfs blueprint (M.empty) startState numSteps 0
--     where startState = emptyResources{oreRobots=1}


computeSolution blueprint numSteps = bfs blueprint (S.fromList [startState]) (S.fromList [startState]) numSteps
    where startState = emptyResources{oreRobots=1}

computeSolution1 inputLines =
    let 
        blueprints = map parseBlueprint inputLines
        solutions = [reallyTrace (show sol) $ seq sol sol | blueprint <- blueprints, let sol = computeSolution blueprint 24]
        qualityIdSum = sum [id * solution | (id, solution) <- zip [1..] solutions]
    in (solutions, qualityIdSum)
    

-- Star 2

computeSolution2 inputLines =
    let 
        blueprints = take 3 $ map parseBlueprint inputLines
        solutions = [reallyTrace (show sol) $ seq sol sol | blueprint <- blueprints, let sol = computeSolution blueprint 32]
    in (solutions, product solutions)


-- Solutions (IO)
solution1 = fmap computeSolution1 inputLines
solution2 = fmap computeSolution2 inputLines

l = lines "Blueprint 1: Each ore robot costs 4 ore. Each clay robot costs 2 ore. Each obsidian robot costs 3 ore and 14 clay. Each geode robot costs 2 ore and 7 obsidian.\nBlueprint 2: Each ore robot costs 2 ore. Each clay robot costs 3 ore. Each obsidian robot costs 3 ore and 8 clay. Each geode robot costs 3 ore and 12 obsidian."

bp = parseBlueprint "Blueprint 2: Each ore robot costs 4 ore. Each clay robot costs 4 ore. Each obsidian robot costs 4 ore and 12 clay. Each geode robot costs 4 ore and 19 obsidian."
