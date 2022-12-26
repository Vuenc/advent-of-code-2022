module Day16 where

-- import Control.Monad.Applicative
-- import Control.Monad.Search
-- import Data.Monoid (Sum(..))
import qualified Data.Map as M
import Algorithm.Search (dijkstra)
import Data.Char (ord)
import Data.Foldable (find, Foldable (toList))
import Debug.Trace (trace)
import Data.Function (on)
import Data.List (intercalate, sortOn)
import Text.Regex.TDFA
import Data.List.Split (splitOn)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe (fromJust)

inputLines = fmap lines $ readFile "inputs/input16.txt"

data Valve = Valve {
    name :: String,
    flowRate :: Int,
    neighbors :: [Valve]
}

-- Valve SZ has flow rate=0; tunnels lead to valves GQ, YZ

instance Show Valve where
    show valve = "[" ++ (name valve) ++ " (" ++ (show $ flowRate valve) ++ ") -> " ++ (intercalate "," [name neighbor | neighbor <- neighbors valve]) ++ "]"
    
instance Eq Valve where
    (==) = (==) `on` name

instance Ord Valve where
    (<=) = (<=) `on` name

-- Star 1

parseInputLine :: String -> (String, Int, [String])
parseInputLine line = (name, read flowRate :: Int, splitOn ", " neighborNames)
    where (_, _, _, (name:flowRate:_:_:_:neighborNames:[])) = (line =~ "Valve ([A-Z]+) has flow rate=([0-9]+); tunne(l|ls) lea(d|ds) to valv(e|es) (.*)") :: (String, String, String, [String])

buildGraph inputLines =
    let vertices = [Valve {name = name, flowRate = flowRate, neighbors = map ((M.!) verticesByName) neighborNames } | line <- inputLines, let (name, flowRate, neighborNames) = parseInputLine line]
        verticesByName = M.fromList [(name valve, valve) | valve <- vertices]
    in vertices

posFlowValvesShortestPaths :: [Valve] -> M.Map Valve [(Valve, Int)]
posFlowValvesShortestPaths valves =
    let posFlowValves = filter ((>0) . flowRate) valves
        distancesListFrom fromValve = sortOn (\(_, dist) -> dist) [(toValve, dist) | toValve <- posFlowValves, toValve /= fromValve, let (dist, _) = shortestPath fromValve toValve]
        Just startValve = find ((=="AA") . name) valves
    in M.fromList (sortOn (\(valve, _) -> - (flowRate valve)) [(fromValve, distancesListFrom fromValve) | fromValve <- (startValve:posFlowValves)])

shortestPath :: Valve -> Valve -> (Int, [Valve])
shortestPath fromValve toValve = let
    transitionCost i j = 1
    solutionFound = (==toValve)
    in fromJust $ dijkstra neighbors transitionCost solutionFound fromValve
        

findSolution shortestPathsMap stepsRemaining currentValve valveKeysVisited currentScore currentMax currentMaxPath currentPath =
    -- from currentValve:
    -- iterate over valvesRemaining
    -- for each, recurse 
    let siblingStep ((valveToCheck, distanceToValveToCheck):valvesWithDistancesToCheck) currentMax currentMaxPath =
            let stepsRemainingAfterOpen = (stepsRemaining - distanceToValveToCheck - 1)
                addedScore = stepsRemainingAfterOpen * (flowRate valveToCheck)
                newPath = ((name valveToCheck, 31 - stepsRemainingAfterOpen, currentScore+addedScore):currentPath)
                (restMax, restMaxPath) = findSolution shortestPathsMap stepsRemainingAfterOpen valveToCheck (S.insert (name valveToCheck) valveKeysVisited) (currentScore + addedScore) currentMax currentMaxPath newPath
                newMax = max currentMax restMax
                newMaxPath = if restMax > currentMax then restMaxPath else currentMaxPath
        -- (siblingsMax, siblingsSolution) = 
            in if stepsRemainingAfterOpen >= 0 then siblingStep valvesWithDistancesToCheck newMax newMaxPath else siblingStep valvesWithDistancesToCheck currentMax currentMaxPath
        siblingStep [] currentMax currentMaxPath = (currentMax, currentMaxPath)
        valvesWithDistancesToCheck = [(valve, dist) :: (Valve, Int) | (valve, dist) <- (M.!)  shortestPathsMap currentValve, not $ S.member (name valve) valveKeysVisited, dist < stepsRemaining]
    in case valvesWithDistancesToCheck of
        [] -> if currentScore > currentMax then trace (show (currentScore, currentPath)) (currentScore, (reverse currentPath)) else (currentMax, currentMaxPath)
        _ -> siblingStep valvesWithDistancesToCheck currentMax currentMaxPath

computeSolution1 inputLines = let
        valves = buildGraph inputLines
        shortestPathsMap = posFlowValvesShortestPaths valves
        Just startValve = find ((=="AA") . name) valves
    in findSolution shortestPathsMap 30 startValve (S.fromList [name startValve]) 0 0 [] [(name startValve, 1, 0)]

-- Star 2


-- findSolutionTwoAgents shortestPathsMap stepsRemaining1 stepsRemaining2 currentValve1 currentValve2 valveKeysVisited currentScore currentMax =
--     -- from currentValve:
--     -- iterate over valvesRemaining
--     -- for each, recurse 
--     let _checkNextStep1 ((valveToCheck, distanceToValveToCheck):valvesWithDistancesToCheck) currentValve2 currentMax =
--             let stepsRemainingAfterOpen = (stepsRemaining - distanceToValveToCheck - 1)
--                 addedScore = stepsRemainingAfterOpen * (flowRate valveToCheck)
--                 restMax = findSolutionTwoAgents shortestPathsMap stepsRemainingAfterOpen stepsRemaining2 valveToCheck currentValve2 (S.insert (name valveToCheck) valveKeysVisited) (currentScore + addedScore) currentMax
--                 newMax = max currentMax restMax
--                 -- (siblingsMax, siblingsSolution) = 
--             in if stepsRemainingAfterOpen >= 0 then _checkNextStep1 valvesWithDistancesToCheck undefined newMax else _checkNextStep valvesWithDistancesToCheck undefined currentMax
--         _checkNextStep1 [] _ currentMax = currentMax
--         valvesWithDistancesToCheck = [(valve, dist) :: (Valve, Int) | (valve, dist) <- (M.!)  shortestPathsMap currentValve1, not $ S.member (name valve) valveKeysVisited, dist < stepsRemaining]
--     in case valvesWithDistancesToCheck of
--         [] -> if currentScore > currentMax then trace (show currentScore) currentScore else currentMax
--         _ -> _checkNextStep1 valvesWithDistancesToCheck undefined currentMax

fakeValve = Valve { name = "XXX", flowRate = 0, neighbors = []}

findSolutionTwoAgents shortestPathsMap (stepsRemaining1, stepsRemaining2) (currentValve1, currentValve2) valveKeysVisited currentScore currentMax =
    -- from currentValve:
    -- iterate over valvesRemaining
    -- for each, recurse 
    let siblingStepTwoAgents (((valveToCheck1, distanceToValveToCheck1), (valveToCheck2, distanceToValveToCheck2)):valvesWithDistancesToCheck) currentMax =
            let stepsRemainingAfterOpen1 = (stepsRemaining1 - distanceToValveToCheck1 - 1)
                stepsRemainingAfterOpen2 = (stepsRemaining2 - distanceToValveToCheck2 - 1)
                addedScore1 = if stepsRemainingAfterOpen1 > 0 then stepsRemainingAfterOpen1 * (flowRate valveToCheck1) else 0
                addedScore2 = if stepsRemainingAfterOpen2 > 0 then stepsRemainingAfterOpen2 * (flowRate valveToCheck2) else 0
                addedScore = addedScore1 + addedScore2
                -- newPath = (((name valveToCheck1, 27 - stepsRemainingAfterOpen1) (name valveToCheck2, 27 - stepsRemainingAfterOpen2), currentScore+addedScore):currentPath)
                restMax = findSolutionTwoAgents shortestPathsMap (stepsRemainingAfterOpen1, stepsRemainingAfterOpen2) (valveToCheck1, valveToCheck2) (S.insert (name valveToCheck1) $ S.insert (name valveToCheck2) valveKeysVisited) (currentScore + addedScore) currentMax
                newMax = max currentMax restMax
                -- newMaxPath = if restMax > currentMax then restMaxPath else currentMaxPath
                -- (siblingsMax, siblingsSolution) = 
                continuingStep stepsRem1 stepsRem2
                    | stepsRem1 <= 0, stepsRem2 <= 0 = siblingStepTwoAgents valvesWithDistancesToCheck currentMax
                    | stepsRem1 <= 0, stepsRem2 > 0 = siblingStepTwoAgents valvesWithDistancesToCheck newMax
                    | stepsRem1 > 0, stepsRem2 <= 0 = siblingStepTwoAgents valvesWithDistancesToCheck newMax
                    | stepsRem1 > 0, stepsRem2 > 0 = siblingStepTwoAgents valvesWithDistancesToCheck newMax
                in continuingStep stepsRemainingAfterOpen1 stepsRemainingAfterOpen2
        siblingStepTwoAgents [] currentMax = currentMax   
        valvesWithDistancesToCheck1 = [(valve, dist) :: (Valve, Int) | (valve, dist) <- (M.!)  shortestPathsMap currentValve1, not $ S.member (name valve) valveKeysVisited, dist < stepsRemaining1]
        valvesWithDistancesToCheck2 = [(valve, dist) :: (Valve, Int) | (valve, dist) <- (M.!)  shortestPathsMap currentValve2, not $ S.member (name valve) valveKeysVisited, dist < stepsRemaining2]
        valvesAtLeastOne1 = if valvesWithDistancesToCheck1 == [] then [(fakeValve,1)] else valvesWithDistancesToCheck1
        valvesAtLeastOne2 = if valvesWithDistancesToCheck2 == [] then [(fakeValve,1)] else valvesWithDistancesToCheck2
        valveCombinations = [(a,b) | a <- valvesAtLeastOne1, b <- valvesAtLeastOne2, (fst a) /= (fst b)]
    in case (valvesWithDistancesToCheck1, valvesWithDistancesToCheck2) of
        ([], []) -> if currentScore > currentMax then trace (show currentScore) currentScore else currentMax
        _ -> siblingStepTwoAgents valveCombinations currentMax -- valvesWithDistancesToCheck currentMax


computeSolution2 inputLines = let
        valves = buildGraph inputLines
        shortestPathsMap = posFlowValvesShortestPaths valves
        shortestPathsMapWithFake = M.insert fakeValve [] shortestPathsMap
        Just startValve = find ((=="AA") . name) valves
    in findSolutionTwoAgents shortestPathsMapWithFake (26, 26) (startValve, startValve) (S.fromList [name startValve]) 0 0

-- Solutions (IO)
solution1 = fmap computeSolution1 inputLines
solution2 = fmap computeSolution2 inputLines

l = lines "Valve AA has flow rate=0; tunnels lead to valves DD, II, BB\nValve BB has flow rate=13; tunnels lead to valves CC, AA\nValve CC has flow rate=2; tunnels lead to valves DD, BB\nValve DD has flow rate=20; tunnels lead to valves CC, AA, EE\nValve EE has flow rate=3; tunnels lead to valves FF, DD\nValve FF has flow rate=0; tunnels lead to valves EE, GG\nValve GG has flow rate=0; tunnels lead to valves FF, HH\nValve HH has flow rate=22; tunnel leads to valve GG\nValve II has flow rate=0; tunnels lead to valves AA, JJ\nValve JJ has flow rate=21; tunnel leads to valve II\n"

ll = lines "Valve SZ has flow rate=0; tunnels lead to valves GQ, YZ\nValve SP has flow rate=0; tunnels lead to valves LJ, AA\nValve LQ has flow rate=0; tunnels lead to valves EY, JT\nValve AT has flow rate=17; tunnels lead to valves DX, BU, NE, BR, TD\nValve IR has flow rate=0; tunnels lead to valves XN, UI\nValve CF has flow rate=0; tunnels lead to valves XN, BR\nValve TE has flow rate=0; tunnels lead to valves YA, RY\nValve GQ has flow rate=22; tunnels lead to valves SZ, AQ, OW, XJ\nValve DX has flow rate=0; tunnels lead to valves HI, AT\nValve AQ has flow rate=0; tunnels lead to valves AZ, GQ\nValve NE has flow rate=0; tunnels lead to valves AT, IA\nValve OC has flow rate=4; tunnels lead to valves PE, QV, QI, LJ, WX\nValve JO has flow rate=0; tunnels lead to valves AA, UI\nValve BR has flow rate=0; tunnels lead to valves CF, AT\nValve ZW has flow rate=0; tunnels lead to valves JH, EY\nValve TD has flow rate=0; tunnels lead to valves AT, WX\nValve BU has flow rate=0; tunnels lead to valves AT, ES\nValve QI has flow rate=0; tunnels lead to valves OC, XN\nValve PE has flow rate=0; tunnels lead to valves CI, OC\nValve WX has flow rate=0; tunnels lead to valves TD, OC\nValve IA has flow rate=0; tunnels lead to valves UI, NE\nValve TR has flow rate=18; tunnel leads to valve HI\nValve JK has flow rate=0; tunnels lead to valves QV, UI\nValve UB has flow rate=0; tunnels lead to valves OM, AA\nValve KW has flow rate=0; tunnels lead to valves YL, MD\nValve AL has flow rate=0; tunnels lead to valves ZL, WZ\nValve VK has flow rate=11; tunnels lead to valves OM, ZL, CI, VA, XJ\nValve FF has flow rate=0; tunnels lead to valves VD, AA\nValve MD has flow rate=0; tunnels lead to valves KW, YA\nValve VA has flow rate=0; tunnels lead to valves AZ, VK\nValve CI has flow rate=0; tunnels lead to valves VK, PE\nValve LJ has flow rate=0; tunnels lead to valves SP, OC\nValve YL has flow rate=23; tunnels lead to valves OW, KW\nValve JH has flow rate=0; tunnels lead to valves RK, ZW\nValve ES has flow rate=13; tunnel leads to valve BU\nValve OM has flow rate=0; tunnels lead to valves UB, VK\nValve QV has flow rate=0; tunnels lead to valves OC, JK\nValve XN has flow rate=7; tunnels lead to valves QI, VD, IR, CF, OG\nValve EY has flow rate=10; tunnels lead to valves ZW, LQ, XC, RC\nValve XJ has flow rate=0; tunnels lead to valves GQ, VK\nValve HI has flow rate=0; tunnels lead to valves DX, TR\nValve VD has flow rate=0; tunnels lead to valves FF, XN\nValve RY has flow rate=0; tunnels lead to valves AZ, TE\nValve YZ has flow rate=0; tunnels lead to valves SZ, YA\nValve YA has flow rate=12; tunnels lead to valves YZ, MD, TE\nValve AZ has flow rate=14; tunnels lead to valves AQ, RC, RY, VA\nValve ZL has flow rate=0; tunnels lead to valves AL, VK\nValve UE has flow rate=0; tunnels lead to valves RK, UI\nValve WZ has flow rate=25; tunnel leads to valve AL\nValve EB has flow rate=0; tunnels lead to valves AA, XC\nValve UI has flow rate=8; tunnels lead to valves UE, JK, IR, JO, IA\nValve AA has flow rate=0; tunnels lead to valves UB, JO, FF, EB, SP\nValve OG has flow rate=0; tunnels lead to valves XN, DF\nValve RC has flow rate=0; tunnels lead to valves AZ, EY\nValve JT has flow rate=21; tunnel leads to valve LQ\nValve DF has flow rate=0; tunnels lead to valves OG, RK\nValve RK has flow rate=9; tunnels lead to valves DF, JH, UE\nValve OW has flow rate=0; tunnels lead to valves YL, GQ\nValve XC has flow rate=0; tunnels lead to valves EY, EB\n"