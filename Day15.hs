module Day15 where

import Text.Regex.TDFA
import Data.List (sort, nub)
import Data.Function (on)
import Debug.Trace (trace)

inputLines = fmap lines $ readFile "inputs/input15.txt"

data Sensor = Sensor {
    sensorX :: Int,
    sensorY :: Int,
    beaconX :: Int,
    beaconY :: Int
} deriving (Show, Eq)

-- Star 1

parseSensorLine :: String -> Sensor
parseSensorLine line = Sensor {sensorX = read sx, sensorY = read sy, beaconX = read bx, beaconY = read by}
    where (_, _, _, [sx, sy, bx, by]) = line =~ "Sensor at x=(.*), y=(.*): closest beacon is at x=(.*), y=(.*)" :: (String, String, String, [String])

projectProximityCellToY y sensor =
    let range = (abs $ sensorX sensor - beaconX sensor) + (abs $ sensorY sensor - beaconY sensor)
        distToRow = (abs $ sensorY sensor - y)
        result
            | distToRow <= range = [(sensorX sensor - (range - distToRow), sensorX sensor + (range - distToRow))]
            | otherwise = []
    in result

intervalUnion :: [(Int, Int)] -> [(Int, Int)]
intervalUnion intervals =
    let sortedIntervals = sort intervals
        reduce inList@((from,to):finishedIntervals) (fromNew, toNew)
            | fromNew > to + 1 = ((fromNew,toNew):inList)
            | toNew > to = (from, toNew):finishedIntervals
            | otherwise = inList
        reduce [] newInterval = [newInterval]
    in reverse $ foldl reduce [] sortedIntervals

computeSolution1 y inputLines =
    let sensors = map parseSensorLine inputLines
        -- xs = (map sensorX sensors) ++ (map beaconX sensors)
        -- ys = (map sensorY sensors) ++ (map beaconY sensors)
        -- (minX, maxX, minY, maxY) = (minimum xs, maximum xs, minimum ys, maximum ys)
        numBeaconsAtY = length $ nub $ filter ((==y) . snd) $ map (\s -> (beaconX s, beaconY s))  sensors
        occupiedIntervals = intervalUnion $ concat $ map (projectProximityCellToY y) sensors
    in sum [(b-a+1) | (a, b) <- occupiedIntervals] - numBeaconsAtY


-- Star 2


computeSolution2 maxXY inputLines =
    let sensors = map parseSensorLine inputLines
        occupiedIntervalsByY = [intervalUnion $ concat $ map (projectProximityCellToY y) sensors | y <- [0..maxXY]]
        relevantIntervals = [(y,(a,b)) | (y, ints) <- zip [0..maxXY] occupiedIntervalsByY, (a,b) <- ints, a > 0 || b < maxXY]
        [(solutionY, (_, solutionXMinusOne)), _] = relevantIntervals
    in (solutionXMinusOne + 1) * maxXY + solutionY


-- Solutions (IO)
solution1 = fmap (computeSolution1 2000000) inputLines
solution2 = fmap (computeSolution2 4000000) inputLines

l = lines "Sensor at x=2, y=18: closest beacon is at x=-2, y=15\nSensor at x=9, y=16: closest beacon is at x=10, y=16\nSensor at x=13, y=2: closest beacon is at x=15, y=3\nSensor at x=12, y=14: closest beacon is at x=10, y=16\nSensor at x=10, y=20: closest beacon is at x=10, y=16\nSensor at x=14, y=17: closest beacon is at x=10, y=16\nSensor at x=8, y=7: closest beacon is at x=2, y=10\nSensor at x=2, y=0: closest beacon is at x=2, y=10\nSensor at x=0, y=11: closest beacon is at x=2, y=10\nSensor at x=20, y=14: closest beacon is at x=25, y=17\nSensor at x=17, y=20: closest beacon is at x=21, y=22\nSensor at x=16, y=7: closest beacon is at x=15, y=3\nSensor at x=14, y=3: closest beacon is at x=15, y=3\nSensor at x=20, y=1: closest beacon is at x=15, y=3"
ll = lines "Sensor at x=1518415, y=2163633: closest beacon is at x=1111304, y=1535696\nSensor at x=2474609, y=3598166: closest beacon is at x=2691247, y=4007257\nSensor at x=426959, y=473371: closest beacon is at x=-529106, y=1145419\nSensor at x=3999598, y=1984775: closest beacon is at x=3975468, y=2000000\nSensor at x=2459256, y=2951561: closest beacon is at x=2132806, y=2866452\nSensor at x=2925882, y=2862933: closest beacon is at x=3325001, y=3024589\nSensor at x=3539174, y=3882566: closest beacon is at x=3132375, y=3541509\nSensor at x=3044887, y=3798155: closest beacon is at x=3132375, y=3541509\nSensor at x=1792818, y=3506985: closest beacon is at x=2132806, y=2866452\nSensor at x=3761945, y=3304667: closest beacon is at x=3325001, y=3024589\nSensor at x=71968, y=3823892: closest beacon is at x=-1085197, y=3401157\nSensor at x=2902345, y=3999748: closest beacon is at x=2691247, y=4007257\nSensor at x=2074989, y=2347435: closest beacon is at x=2132806, y=2866452\nSensor at x=1115220, y=1782338: closest beacon is at x=1111304, y=1535696\nSensor at x=369130, y=2348958: closest beacon is at x=1111304, y=1535696\nSensor at x=2525090, y=1917940: closest beacon is at x=2603675, y=2276026\nSensor at x=2861163, y=3386968: closest beacon is at x=3132375, y=3541509\nSensor at x=3995081, y=2010596: closest beacon is at x=3975468, y=2000000\nSensor at x=3038274, y=534921: closest beacon is at x=4354209, y=-17303\nSensor at x=3646366, y=2868267: closest beacon is at x=3325001, y=3024589\nSensor at x=3308360, y=1653497: closest beacon is at x=3975468, y=2000000\nSensor at x=1996072, y=995783: closest beacon is at x=1111304, y=1535696\nSensor at x=3852158, y=950900: closest beacon is at x=3975468, y=2000000\nSensor at x=3061849, y=2428914: closest beacon is at x=2603675, y=2276026\nSensor at x=2788254, y=3983003: closest beacon is at x=2691247, y=4007257\nSensor at x=694411, y=1882565: closest beacon is at x=1111304, y=1535696\nSensor at x=2647250, y=2551966: closest beacon is at x=2603675, y=2276026\nSensor at x=1079431, y=3166226: closest beacon is at x=2132806, y=2866452\nSensor at x=3929172, y=2196495: closest beacon is at x=3975468, y=2000000\nSensor at x=3883296, y=2487406: closest beacon is at x=3975468, y=2000000\nSensor at x=1271911, y=1529880: closest beacon is at x=1111304, y=1535696\n"
