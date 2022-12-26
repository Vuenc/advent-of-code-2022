module Day2 where

inputLines = fmap lines $ readFile "inputs/input2.txt"

winPoints "A Y" = 6
winPoints "B Z" = 6
winPoints "C X" = 6
winPoints "A X" = 3
winPoints "B Y" = 3
winPoints "C Z" = 3
winPoints _ = 0

shapePoints (_:_:"X") = 1
shapePoints (_:_:"Y") = 2
shapePoints (_:_:"Z") = 3

totalPoints round = winPoints round + shapePoints round

computeSolution1 inputLines =
    let pointsPerRound = map totalPoints inputLines
    in sum pointsPerRound 

solution1 = fmap computeSolution1 inputLines

part2ToPart1Code "A X" = "A Z"
part2ToPart1Code "A Y" = "A X"
part2ToPart1Code "A Z" = "A Y"
part2ToPart1Code "B X" = "B X"
part2ToPart1Code "B Y" = "B Y"
part2ToPart1Code "B Z" = "B Z"
part2ToPart1Code "C X" = "C Y"
part2ToPart1Code "C Y" = "C Z"
part2ToPart1Code "C Z" = "C X"

computeSolution2 inputLines =
    let pointsPerRound = map (totalPoints . part2ToPart1Code) inputLines
    in sum pointsPerRound

solution2 = fmap computeSolution2 inputLines