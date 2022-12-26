module Day25 where

import Numeric (showIntAtBase)

inputLines = fmap lines $ readFile "inputs/input25.txt"

-- Star 1

parseSnafu line =
    let powers = reverse $ take (length line) $ (iterate (*5) 1)
    in sum $ map (\(c, p) -> (parseSnafuChar c) * p) $ zip line powers

parseSnafuChar '-' = -1
parseSnafuChar '=' = -2
parseSnafuChar c = read [c]

positivizedDigitToSnafu 0 = '='
positivizedDigitToSnafu 1 = '-'
positivizedDigitToSnafu 2 = '0'
positivizedDigitToSnafu 3 = '1'
positivizedDigitToSnafu 4 = '2'

numberToSnafu num =
    let digitLength = length $ showIntAtBase 5 (head . show) num ""
        addNum = 2 * (sum $ take digitLength (iterate (*5) 1))
    in showIntAtBase 5 positivizedDigitToSnafu (num + addNum) ""

computeSolution1 inputLines = numberToSnafu $ sum $ map parseSnafu inputLines

-- Star 2


computeSolution2 inputLines = ""


-- Solutions (IO)
solution1 = fmap computeSolution1 inputLines
solution2 = fmap computeSolution2 inputLines
