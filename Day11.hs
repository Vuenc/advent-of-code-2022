module Day11 where
import Text.Regex.TDFA
import Data.List.Split (chunksOf, splitOn)
import qualified Data.Map as M
import Data.Map ((!))
import Day12 (l)
import Data.List (sortOn)
import Debug.Trace (trace)

inputLines = fmap lines $ readFile "inputs/input11.txt"

data Monkey = Monkey {
    items :: [Int],
    operation :: Int -> Int,
    testDivisor :: Int,
    targetMonkeyTrue :: Int,
    targetMonkeyFalse :: Int,
    itemsInspected :: Int
}

instance Show Monkey where
    show monkey = "Monkey: " ++ (show $ items monkey) ++ " (inspected " ++ (show $ itemsInspected monkey) ++ " items)\n"

getMonkeys inputLines = map (getMonkey . take 6) $ chunksOf 7 inputLines

getMonkey :: [String] -> Monkey
getMonkey [_, startingItemsLine, operationLine, testLine, testTrueLine, testFalseLine] =
    let (_, _, _, [startingItemsStr]) = (startingItemsLine =~ "Starting items: (.*)") :: (String, String, String, [String])
        (_, _, _, [op1, operator, op2]) = (operationLine =~ "Operation: new = (old|[0-9]*) (.) (old|[0-9]*)") :: (String, String, String, [String])
        (_, _, _, [divisor]) = (testLine =~ "Test: divisible by ([0-9]*)") :: (String, String, String, [String])
        (_, _, _, [targetTrue]) = (testTrueLine =~ "If true: throw to monkey ([0-9]*)") :: (String, String, String, [String])
        (_, _, _, [targetFalse]) = (testFalseLine =~ "If false: throw to monkey ([0-9]*)") :: (String, String, String, [String])
        startingItems = reverse $ map read $ splitOn ", " startingItemsStr
        operation = parseOperation op1 operator op2
    in Monkey { items=startingItems, operation=operation, testDivisor=read divisor, targetMonkeyTrue=read targetTrue, targetMonkeyFalse=read targetFalse, itemsInspected = 0}

simulateOneMonkey monkeysMap monkeyIndex worryDivisor worryModulo =
    let monkey = monkeysMap ! monkeyIndex
        tmpItems = map (operation monkey) (reverse $ items monkey)
        newItems = map ((`div` worryDivisor) . (`mod` worryModulo)) tmpItems
        targetMonkeys = map (\x -> if x `mod` (testDivisor monkey) == 0 then (targetMonkeyTrue monkey) else (targetMonkeyFalse monkey)) (newItems)
        newMonkeysMap = foldl (\monkMap (target, item) -> M.insert target (monkeyAddItem (monkMap ! target) item) monkMap) monkeysMap (zip targetMonkeys newItems)
        newMonkey = monkey { items=[], itemsInspected=(itemsInspected monkey) + length (items monkey) }
    in M.insert monkeyIndex newMonkey newMonkeysMap

simulateOneRound numMonkeys currentIndex worryDivisor worryModulo monkeysMap
    | currentIndex < numMonkeys = simulateOneRound numMonkeys (currentIndex + 1) worryDivisor worryModulo (simulateOneMonkey monkeysMap currentIndex worryDivisor worryModulo)
    | otherwise = monkeysMap

simulateNRounds monkeysMap n worryDivisor worryModulo = (iterate (simulateOneRound (length monkeysMap) 0 worryDivisor worryModulo) monkeysMap) !! n


monkeyAddItem monkey item = monkey { items=(item:(items monkey)) }

parseOperation a op b x = (if a == "old" then x else read a) `operation` (if b == "old" then x else read b)
    where operation = if op == "+" then (+) else (*)

-- Star 1


computeSolution1 inputLines =
    let monkeys = getMonkeys inputLines
        monkeysMap = M.fromList $ zip [0..] $ getMonkeys inputLines
        -- endResult = simulateOneMonkey monkeysMap 0
        worryModulo = product $ map testDivisor monkeys
        endResult = simulateNRounds monkeysMap 20 3 worryModulo
        (a:b:_) = sortOn (negate) $ map (itemsInspected . snd) $ M.toList endResult
    in a * b

-- Star 2


computeSolution2 inputLines =
    let monkeys = getMonkeys inputLines
        monkeysMap = M.fromList $ zip [0..] $ monkeys
        -- endResult = simulateOneMonkey monkeysMap 0
        worryModulo = product $ map testDivisor monkeys
        endResult = simulateNRounds monkeysMap 10000 1 worryModulo
        (a:b:_) = sortOn (negate) $ map (itemsInspected . snd) $ M.toList endResult
    in a * b



-- Solutions (IO)
solution1 = fmap computeSolution1 inputLines
solution2 = fmap computeSolution2 inputLines

l = lines "Monkey 0:\n  Starting items: 79, 98\n  Operation: new = old * 19\n  Test: divisible by 23\n    If true: throw to monkey 2\n    If false: throw to monkey 3\n\nMonkey 1:\n  Starting items: 54, 65, 75, 74\n  Operation: new = old + 6\n  Test: divisible by 19\n    If true: throw to monkey 2\n    If false: throw to monkey 0\n\nMonkey 2:\n  Starting items: 79, 60, 97\n  Operation: new = old * old\n  Test: divisible by 13\n    If true: throw to monkey 1\n    If false: throw to monkey 3\n\nMonkey 3:\n  Starting items: 74\n  Operation: new = old + 3\n  Test: divisible by 17\n    If true: throw to monkey 0\n    If false: throw to monkey 1"
