module Day14 where

import Data.List.Split
import Data.List (foldl')
-- import qualified Data.Array.Repa as R
import qualified Data.Map as M
import Data.Bits (Bits(xor))

inputLines = fmap lines $ readFile "inputs/input14.txt"

-- type Array2D = [[Char]]
-- type Array2D = R.Array R.U R.DIM2 Char
type Array2D = M.Map (Int, Int) Char

readAt :: Array2D -> (Int, Int) -> Char
-- readAt array (i,j) = (array !! j) !! i
-- readAt array (i,j) = R.index array (R.Z R.:. i R.:. j)
readAt array (i,j) = case M.lookup (i,j) array of
  Just x -> x
  Nothing -> '.'

writeAt :: Array2D -> (Int, Int) -> Char -> Array2D
-- writeAt array (i,j) val = seq newArray newArray
--   where oldRow = array !! j
--         newRow = (take i oldRow) ++ [val] ++ (drop (i+1) oldRow)
--         newArray = (take j array) ++ [newRow] ++ (drop (j+1) array)
writeAt array (i,j) '.' = M.delete (i,j) array
writeAt array (i,j) x = M.insert (i,j) x array

-- Star 1

wallIndicesLine :: [Char] -> [(Int, Int)]
wallIndicesLine line = concat $ map lineFromPair $ pairs
    where coords = map (map read) $ map (splitOn ",") $ splitOn " -> " line
          pairs = zip coords (tail coords)
          lineFromPair ([x1,y1], [x2,y2])
            | x1 == x2 = [(x1,y) | y <- [(min y1 y2)..(max y1 y2)]]
            | y1 == y2 = [(x,y1) | x <- [(min x1 x2)..(max x1 x2)]]


wallIndices inputLines = concat $ map wallIndicesLine inputLines

wallIndicesNorm wallInds xOffset = [(x-xOffset+1, y) | (x,y) <- wallInds]

sandSourceNorm xOffset = (500 - xOffset + 1, 0)

extents wallInds = (minX, minY, maxX, maxY)
  where maxX = maximum [x | (x, _) <- wallInds]
        maxY = maximum [y | (_, y) <- wallInds]
        minX = minimum [x | (x, _) <- wallInds]
        minY = minimum [y | (_, y) <- wallInds]

field wallInds hasVoid = foldl (\array (x,y) -> writeAt array (x,y) '#') emptyField wallIndsNorm
  where wallIndsNorm = wallIndicesNorm wallInds xFrom
        (xFrom, xTo) = (min minX (500 - maxY - 5), max maxX (500 + maxY + 5))
        ext@(minX, minY, maxX, maxY) = extents wallInds
        _emptyField
          | hasVoid   = [['~'] ++ ['.' | _ <- [xFrom..xTo]] ++ ['~'] | _ <- [0..maxY]] ++ [(take (xTo-xFrom+3) $ repeat '~')]
          | otherwise = [['.'] ++ ['.' | _ <- [xFrom..xTo]] ++ ['.'] | _ <- [0..maxY]] ++ [(take (xTo-xFrom+3) $ repeat '.')] ++ [(take (xTo-xFrom+3) $ repeat '#')]
        emptyField = foldl' (\array (x,y,entry) -> writeAt array (x,y) entry) (M.empty) [(x,y,entry) | (y, row) <- zip [0..] _emptyField, (x, entry) <- zip [0..] row]

data SandState = Moving | Resting | Lost deriving (Eq)

map3x1 [   d, '.',   f] = ([   d, 'o',   f], (0, 1), Moving)
map3x1 [ '.',   e,   f] = ([ 'o',   e,   f], (-1, 1), Moving)
map3x1 [   d,   e, '.'] = ([   d,   e, 'o'], (1, 1), Moving)
map3x1 [   d,   e,   f]
  | d == '~' || e == '~' || f == '~' =        ([   d,   e,   f], (0, 1), Lost)
  | otherwise =                          ([   d,   e,   f], (0, 0), Resting)

simulateSandStep field (sandX, sandY) =
  let patch3x1 = [(sandX-1, sandY+1), (sandX, sandY+1), (sandX+1, sandY+1)]
      oldVals = [readAt field xy | xy <- patch3x1]
      (newVals, (dX, dY), sandState) = map3x1 oldVals
      fieldSandChanged
        | sandState == Resting = field
        | otherwise = writeAt field (sandX, sandY) '.'
      finalField = foldl' (\array ((x,y), newVal, oldVal) -> if oldVal /= newVal then writeAt array (x,y) newVal else array) fieldSandChanged (zip3 patch3x1 newVals oldVals)
      newSandPos = (sandX + dX, sandY + dY)
      returnVal = (finalField, newSandPos, sandState)
  in seq returnVal returnVal

simulateOneSandToCompletion field sandSource =
  let fieldWithSand = writeAt field sandSource 'o'
      steps = iterate (\(field, sandXY, sandFinished) -> simulateSandStep field sandXY) (fieldWithSand, sandSource, Moving)
      (finalField, _, sandState) = head $ dropWhile (\(_, _, sandState) -> sandState == Moving) steps
      returnVal = (finalField, sandState == Resting)
  in seq returnVal returnVal

sandStepsToCompletion field sandSource maxSteps hasVoid = 
  let steps = iterate (\(field, _) -> simulateOneSandToCompletion field sandSource) (field, True)
      (i, (finalField, _))
        | maxSteps == (-1) && hasVoid = head $ dropWhile (\(_, (_, sandResting)) -> sandResting) $ zip [0..] steps
        | maxSteps == (-1) && not hasVoid = head $ dropWhile (\(_, (field, _)) -> (readAt field sandSource /= 'o')) $ zip [0..] steps
        | maxSteps > 0 = head $ drop maxSteps $ zip [0..] steps
      returnVal = (i - (if hasVoid then 1 else 0), finalField)
  in seq returnVal returnVal

computeSolution maxSteps hasVoid inputLines =
  let wallInds  = wallIndices inputLines
      initialField = field wallInds hasVoid
      (minX, minY, maxX, maxY) = extents wallInds
      (xFrom, xTo) = (min minX (500 - maxY - 5), max maxX (500 + maxY + 5))
      sandSource = sandSourceNorm xFrom
  in (sandStepsToCompletion initialField sandSource maxSteps hasVoid)

printSolution (i, field) = do
    mapM_ putStrLn field
    putStrLn ""
    print i

computeSolution1 inputLines = let (i, _) = computeSolution (-1) True inputLines in i

l = ["498,4 -> 498,6 -> 496,6", "503,4 -> 502,4 -> 502,9 -> 494,9"]

-- Star 2


computeSolution2 inputLines = let (i, _) = computeSolution (-1) False inputLines in i

-- Solutions (IO)
solution1 = fmap computeSolution1 inputLines
solution2 = fmap computeSolution2 inputLines

ll = lines "477,140 -> 481,140\n468,149 -> 472,149\n482,30 -> 482,23 -> 482,30 -> 484,30 -> 484,20 -> 484,30 -> 486,30 -> 486,26 -> 486,30 -> 488,30 -> 488,23 -> 488,30 -> 490,30 -> 490,20 -> 490,30 -> 492,30 -> 492,25 -> 492,30 -> 494,30 -> 494,29 -> 494,30 -> 496,30 -> 496,24 -> 496,30\n465,152 -> 469,152\n482,30 -> 482,23 -> 482,30 -> 484,30 -> 484,20 -> 484,30 -> 486,30 -> 486,26 -> 486,30 -> 488,30 -> 488,23 -> 488,30 -> 490,30 -> 490,20 -> 490,30 -> 492,30 -> 492,25 -> 492,30 -> 494,30 -> 494,29 -> 494,30 -> 496,30 -> 496,24 -> 496,30\n501,140 -> 505,140\n490,43 -> 490,42 -> 490,43 -> 492,43 -> 492,35 -> 492,43 -> 494,43 -> 494,38 -> 494,43 -> 496,43 -> 496,36 -> 496,43 -> 498,43 -> 498,39 -> 498,43 -> 500,43 -> 500,35 -> 500,43 -> 502,43 -> 502,36 -> 502,43\n498,17 -> 503,17\n504,68 -> 504,59 -> 504,68 -> 506,68 -> 506,60 -> 506,68 -> 508,68 -> 508,63 -> 508,68 -> 510,68 -> 510,67 -> 510,68\n492,131 -> 496,131\n490,43 -> 490,42 -> 490,43 -> 492,43 -> 492,35 -> 492,43 -> 494,43 -> 494,38 -> 494,43 -> 496,43 -> 496,36 -> 496,43 -> 498,43 -> 498,39 -> 498,43 -> 500,43 -> 500,35 -> 500,43 -> 502,43 -> 502,36 -> 502,43\n482,30 -> 482,23 -> 482,30 -> 484,30 -> 484,20 -> 484,30 -> 486,30 -> 486,26 -> 486,30 -> 488,30 -> 488,23 -> 488,30 -> 490,30 -> 490,20 -> 490,30 -> 492,30 -> 492,25 -> 492,30 -> 494,30 -> 494,29 -> 494,30 -> 496,30 -> 496,24 -> 496,30\n501,116 -> 505,116\n495,122 -> 499,122\n486,131 -> 490,131\n462,155 -> 462,157 -> 457,157 -> 457,162 -> 471,162 -> 471,157 -> 465,157 -> 465,155\n514,84 -> 514,79 -> 514,84 -> 516,84 -> 516,78 -> 516,84 -> 518,84 -> 518,77 -> 518,84 -> 520,84 -> 520,82 -> 520,84 -> 522,84 -> 522,79 -> 522,84\n504,104 -> 504,105 -> 521,105 -> 521,104\n503,70 -> 503,71 -> 518,71\n501,15 -> 506,15\n514,84 -> 514,79 -> 514,84 -> 516,84 -> 516,78 -> 516,84 -> 518,84 -> 518,77 -> 518,84 -> 520,84 -> 520,82 -> 520,84 -> 522,84 -> 522,79 -> 522,84\n504,119 -> 508,119\n529,93 -> 529,95 -> 526,95 -> 526,100 -> 541,100 -> 541,95 -> 533,95 -> 533,93\n482,30 -> 482,23 -> 482,30 -> 484,30 -> 484,20 -> 484,30 -> 486,30 -> 486,26 -> 486,30 -> 488,30 -> 488,23 -> 488,30 -> 490,30 -> 490,20 -> 490,30 -> 492,30 -> 492,25 -> 492,30 -> 494,30 -> 494,29 -> 494,30 -> 496,30 -> 496,24 -> 496,30\n514,84 -> 514,79 -> 514,84 -> 516,84 -> 516,78 -> 516,84 -> 518,84 -> 518,77 -> 518,84 -> 520,84 -> 520,82 -> 520,84 -> 522,84 -> 522,79 -> 522,84\n462,155 -> 462,157 -> 457,157 -> 457,162 -> 471,162 -> 471,157 -> 465,157 -> 465,155\n471,146 -> 475,146\n502,46 -> 502,48 -> 494,48 -> 494,55 -> 507,55 -> 507,48 -> 506,48 -> 506,46\n514,84 -> 514,79 -> 514,84 -> 516,84 -> 516,78 -> 516,84 -> 518,84 -> 518,77 -> 518,84 -> 520,84 -> 520,82 -> 520,84 -> 522,84 -> 522,79 -> 522,84\n502,46 -> 502,48 -> 494,48 -> 494,55 -> 507,55 -> 507,48 -> 506,48 -> 506,46\n495,134 -> 499,134\n482,30 -> 482,23 -> 482,30 -> 484,30 -> 484,20 -> 484,30 -> 486,30 -> 486,26 -> 486,30 -> 488,30 -> 488,23 -> 488,30 -> 490,30 -> 490,20 -> 490,30 -> 492,30 -> 492,25 -> 492,30 -> 494,30 -> 494,29 -> 494,30 -> 496,30 -> 496,24 -> 496,30\n490,43 -> 490,42 -> 490,43 -> 492,43 -> 492,35 -> 492,43 -> 494,43 -> 494,38 -> 494,43 -> 496,43 -> 496,36 -> 496,43 -> 498,43 -> 498,39 -> 498,43 -> 500,43 -> 500,35 -> 500,43 -> 502,43 -> 502,36 -> 502,43\n482,30 -> 482,23 -> 482,30 -> 484,30 -> 484,20 -> 484,30 -> 486,30 -> 486,26 -> 486,30 -> 488,30 -> 488,23 -> 488,30 -> 490,30 -> 490,20 -> 490,30 -> 492,30 -> 492,25 -> 492,30 -> 494,30 -> 494,29 -> 494,30 -> 496,30 -> 496,24 -> 496,30\n498,125 -> 502,125\n490,43 -> 490,42 -> 490,43 -> 492,43 -> 492,35 -> 492,43 -> 494,43 -> 494,38 -> 494,43 -> 496,43 -> 496,36 -> 496,43 -> 498,43 -> 498,39 -> 498,43 -> 500,43 -> 500,35 -> 500,43 -> 502,43 -> 502,36 -> 502,43\n501,122 -> 505,122\n495,140 -> 499,140\n520,108 -> 520,110 -> 516,110 -> 516,113 -> 529,113 -> 529,110 -> 525,110 -> 525,108\n490,43 -> 490,42 -> 490,43 -> 492,43 -> 492,35 -> 492,43 -> 494,43 -> 494,38 -> 494,43 -> 496,43 -> 496,36 -> 496,43 -> 498,43 -> 498,39 -> 498,43 -> 500,43 -> 500,35 -> 500,43 -> 502,43 -> 502,36 -> 502,43\n502,46 -> 502,48 -> 494,48 -> 494,55 -> 507,55 -> 507,48 -> 506,48 -> 506,46\n498,119 -> 502,119\n482,30 -> 482,23 -> 482,30 -> 484,30 -> 484,20 -> 484,30 -> 486,30 -> 486,26 -> 486,30 -> 488,30 -> 488,23 -> 488,30 -> 490,30 -> 490,20 -> 490,30 -> 492,30 -> 492,25 -> 492,30 -> 494,30 -> 494,29 -> 494,30 -> 496,30 -> 496,24 -> 496,30\n482,30 -> 482,23 -> 482,30 -> 484,30 -> 484,20 -> 484,30 -> 486,30 -> 486,26 -> 486,30 -> 488,30 -> 488,23 -> 488,30 -> 490,30 -> 490,20 -> 490,30 -> 492,30 -> 492,25 -> 492,30 -> 494,30 -> 494,29 -> 494,30 -> 496,30 -> 496,24 -> 496,30\n490,43 -> 490,42 -> 490,43 -> 492,43 -> 492,35 -> 492,43 -> 494,43 -> 494,38 -> 494,43 -> 496,43 -> 496,36 -> 496,43 -> 498,43 -> 498,39 -> 498,43 -> 500,43 -> 500,35 -> 500,43 -> 502,43 -> 502,36 -> 502,43\n482,30 -> 482,23 -> 482,30 -> 484,30 -> 484,20 -> 484,30 -> 486,30 -> 486,26 -> 486,30 -> 488,30 -> 488,23 -> 488,30 -> 490,30 -> 490,20 -> 490,30 -> 492,30 -> 492,25 -> 492,30 -> 494,30 -> 494,29 -> 494,30 -> 496,30 -> 496,24 -> 496,30\n490,43 -> 490,42 -> 490,43 -> 492,43 -> 492,35 -> 492,43 -> 494,43 -> 494,38 -> 494,43 -> 496,43 -> 496,36 -> 496,43 -> 498,43 -> 498,39 -> 498,43 -> 500,43 -> 500,35 -> 500,43 -> 502,43 -> 502,36 -> 502,43\n514,84 -> 514,79 -> 514,84 -> 516,84 -> 516,78 -> 516,84 -> 518,84 -> 518,77 -> 518,84 -> 520,84 -> 520,82 -> 520,84 -> 522,84 -> 522,79 -> 522,84\n482,30 -> 482,23 -> 482,30 -> 484,30 -> 484,20 -> 484,30 -> 486,30 -> 486,26 -> 486,30 -> 488,30 -> 488,23 -> 488,30 -> 490,30 -> 490,20 -> 490,30 -> 492,30 -> 492,25 -> 492,30 -> 494,30 -> 494,29 -> 494,30 -> 496,30 -> 496,24 -> 496,30\n498,137 -> 502,137\n474,149 -> 478,149\n514,84 -> 514,79 -> 514,84 -> 516,84 -> 516,78 -> 516,84 -> 518,84 -> 518,77 -> 518,84 -> 520,84 -> 520,82 -> 520,84 -> 522,84 -> 522,79 -> 522,84\n517,90 -> 531,90\n504,68 -> 504,59 -> 504,68 -> 506,68 -> 506,60 -> 506,68 -> 508,68 -> 508,63 -> 508,68 -> 510,68 -> 510,67 -> 510,68\n504,68 -> 504,59 -> 504,68 -> 506,68 -> 506,60 -> 506,68 -> 508,68 -> 508,63 -> 508,68 -> 510,68 -> 510,67 -> 510,68\n504,68 -> 504,59 -> 504,68 -> 506,68 -> 506,60 -> 506,68 -> 508,68 -> 508,63 -> 508,68 -> 510,68 -> 510,67 -> 510,68\n514,84 -> 514,79 -> 514,84 -> 516,84 -> 516,78 -> 516,84 -> 518,84 -> 518,77 -> 518,84 -> 520,84 -> 520,82 -> 520,84 -> 522,84 -> 522,79 -> 522,84\n483,152 -> 487,152\n490,43 -> 490,42 -> 490,43 -> 492,43 -> 492,35 -> 492,43 -> 494,43 -> 494,38 -> 494,43 -> 496,43 -> 496,36 -> 496,43 -> 498,43 -> 498,39 -> 498,43 -> 500,43 -> 500,35 -> 500,43 -> 502,43 -> 502,36 -> 502,43\n483,134 -> 487,134\n490,43 -> 490,42 -> 490,43 -> 492,43 -> 492,35 -> 492,43 -> 494,43 -> 494,38 -> 494,43 -> 496,43 -> 496,36 -> 496,43 -> 498,43 -> 498,39 -> 498,43 -> 500,43 -> 500,35 -> 500,43 -> 502,43 -> 502,36 -> 502,43\n489,134 -> 493,134\n482,30 -> 482,23 -> 482,30 -> 484,30 -> 484,20 -> 484,30 -> 486,30 -> 486,26 -> 486,30 -> 488,30 -> 488,23 -> 488,30 -> 490,30 -> 490,20 -> 490,30 -> 492,30 -> 492,25 -> 492,30 -> 494,30 -> 494,29 -> 494,30 -> 496,30 -> 496,24 -> 496,30\n477,146 -> 481,146\n462,155 -> 462,157 -> 457,157 -> 457,162 -> 471,162 -> 471,157 -> 465,157 -> 465,155\n514,84 -> 514,79 -> 514,84 -> 516,84 -> 516,78 -> 516,84 -> 518,84 -> 518,77 -> 518,84 -> 520,84 -> 520,82 -> 520,84 -> 522,84 -> 522,79 -> 522,84\n462,167 -> 462,168 -> 474,168 -> 474,167\n490,43 -> 490,42 -> 490,43 -> 492,43 -> 492,35 -> 492,43 -> 494,43 -> 494,38 -> 494,43 -> 496,43 -> 496,36 -> 496,43 -> 498,43 -> 498,39 -> 498,43 -> 500,43 -> 500,35 -> 500,43 -> 502,43 -> 502,36 -> 502,43\n520,108 -> 520,110 -> 516,110 -> 516,113 -> 529,113 -> 529,110 -> 525,110 -> 525,108\n490,43 -> 490,42 -> 490,43 -> 492,43 -> 492,35 -> 492,43 -> 494,43 -> 494,38 -> 494,43 -> 496,43 -> 496,36 -> 496,43 -> 498,43 -> 498,39 -> 498,43 -> 500,43 -> 500,35 -> 500,43 -> 502,43 -> 502,36 -> 502,43\n504,68 -> 504,59 -> 504,68 -> 506,68 -> 506,60 -> 506,68 -> 508,68 -> 508,63 -> 508,68 -> 510,68 -> 510,67 -> 510,68\n504,68 -> 504,59 -> 504,68 -> 506,68 -> 506,60 -> 506,68 -> 508,68 -> 508,63 -> 508,68 -> 510,68 -> 510,67 -> 510,68\n520,108 -> 520,110 -> 516,110 -> 516,113 -> 529,113 -> 529,110 -> 525,110 -> 525,108\n514,84 -> 514,79 -> 514,84 -> 516,84 -> 516,78 -> 516,84 -> 518,84 -> 518,77 -> 518,84 -> 520,84 -> 520,82 -> 520,84 -> 522,84 -> 522,79 -> 522,84\n482,30 -> 482,23 -> 482,30 -> 484,30 -> 484,20 -> 484,30 -> 486,30 -> 486,26 -> 486,30 -> 488,30 -> 488,23 -> 488,30 -> 490,30 -> 490,20 -> 490,30 -> 492,30 -> 492,25 -> 492,30 -> 494,30 -> 494,29 -> 494,30 -> 496,30 -> 496,24 -> 496,30\n482,30 -> 482,23 -> 482,30 -> 484,30 -> 484,20 -> 484,30 -> 486,30 -> 486,26 -> 486,30 -> 488,30 -> 488,23 -> 488,30 -> 490,30 -> 490,20 -> 490,30 -> 492,30 -> 492,25 -> 492,30 -> 494,30 -> 494,29 -> 494,30 -> 496,30 -> 496,24 -> 496,30\n482,30 -> 482,23 -> 482,30 -> 484,30 -> 484,20 -> 484,30 -> 486,30 -> 486,26 -> 486,30 -> 488,30 -> 488,23 -> 488,30 -> 490,30 -> 490,20 -> 490,30 -> 492,30 -> 492,25 -> 492,30 -> 494,30 -> 494,29 -> 494,30 -> 496,30 -> 496,24 -> 496,30\n529,93 -> 529,95 -> 526,95 -> 526,100 -> 541,100 -> 541,95 -> 533,95 -> 533,93\n474,143 -> 478,143\n497,13 -> 502,13\n529,93 -> 529,95 -> 526,95 -> 526,100 -> 541,100 -> 541,95 -> 533,95 -> 533,93\n482,30 -> 482,23 -> 482,30 -> 484,30 -> 484,20 -> 484,30 -> 486,30 -> 486,26 -> 486,30 -> 488,30 -> 488,23 -> 488,30 -> 490,30 -> 490,20 -> 490,30 -> 492,30 -> 492,25 -> 492,30 -> 494,30 -> 494,29 -> 494,30 -> 496,30 -> 496,24 -> 496,30\n482,30 -> 482,23 -> 482,30 -> 484,30 -> 484,20 -> 484,30 -> 486,30 -> 486,26 -> 486,30 -> 488,30 -> 488,23 -> 488,30 -> 490,30 -> 490,20 -> 490,30 -> 492,30 -> 492,25 -> 492,30 -> 494,30 -> 494,29 -> 494,30 -> 496,30 -> 496,24 -> 496,30\n505,17 -> 510,17\n529,93 -> 529,95 -> 526,95 -> 526,100 -> 541,100 -> 541,95 -> 533,95 -> 533,93\n462,155 -> 462,157 -> 457,157 -> 457,162 -> 471,162 -> 471,157 -> 465,157 -> 465,155\n477,152 -> 481,152\n462,167 -> 462,168 -> 474,168 -> 474,167\n504,104 -> 504,105 -> 521,105 -> 521,104\n489,128 -> 493,128\n520,108 -> 520,110 -> 516,110 -> 516,113 -> 529,113 -> 529,110 -> 525,110 -> 525,108\n482,30 -> 482,23 -> 482,30 -> 484,30 -> 484,20 -> 484,30 -> 486,30 -> 486,26 -> 486,30 -> 488,30 -> 488,23 -> 488,30 -> 490,30 -> 490,20 -> 490,30 -> 492,30 -> 492,25 -> 492,30 -> 494,30 -> 494,29 -> 494,30 -> 496,30 -> 496,24 -> 496,30\n502,46 -> 502,48 -> 494,48 -> 494,55 -> 507,55 -> 507,48 -> 506,48 -> 506,46\n529,93 -> 529,95 -> 526,95 -> 526,100 -> 541,100 -> 541,95 -> 533,95 -> 533,93\n490,43 -> 490,42 -> 490,43 -> 492,43 -> 492,35 -> 492,43 -> 494,43 -> 494,38 -> 494,43 -> 496,43 -> 496,36 -> 496,43 -> 498,43 -> 498,39 -> 498,43 -> 500,43 -> 500,35 -> 500,43 -> 502,43 -> 502,36 -> 502,43\n482,30 -> 482,23 -> 482,30 -> 484,30 -> 484,20 -> 484,30 -> 486,30 -> 486,26 -> 486,30 -> 488,30 -> 488,23 -> 488,30 -> 490,30 -> 490,20 -> 490,30 -> 492,30 -> 492,25 -> 492,30 -> 494,30 -> 494,29 -> 494,30 -> 496,30 -> 496,24 -> 496,30\n514,84 -> 514,79 -> 514,84 -> 516,84 -> 516,78 -> 516,84 -> 518,84 -> 518,77 -> 518,84 -> 520,84 -> 520,82 -> 520,84 -> 522,84 -> 522,79 -> 522,84\n504,68 -> 504,59 -> 504,68 -> 506,68 -> 506,60 -> 506,68 -> 508,68 -> 508,63 -> 508,68 -> 510,68 -> 510,67 -> 510,68\n483,140 -> 487,140\n529,93 -> 529,95 -> 526,95 -> 526,100 -> 541,100 -> 541,95 -> 533,95 -> 533,93\n482,30 -> 482,23 -> 482,30 -> 484,30 -> 484,20 -> 484,30 -> 486,30 -> 486,26 -> 486,30 -> 488,30 -> 488,23 -> 488,30 -> 490,30 -> 490,20 -> 490,30 -> 492,30 -> 492,25 -> 492,30 -> 494,30 -> 494,29 -> 494,30 -> 496,30 -> 496,24 -> 496,30\n514,84 -> 514,79 -> 514,84 -> 516,84 -> 516,78 -> 516,84 -> 518,84 -> 518,77 -> 518,84 -> 520,84 -> 520,82 -> 520,84 -> 522,84 -> 522,79 -> 522,84\n507,122 -> 511,122\n489,140 -> 493,140\n510,125 -> 514,125\n490,43 -> 490,42 -> 490,43 -> 492,43 -> 492,35 -> 492,43 -> 494,43 -> 494,38 -> 494,43 -> 496,43 -> 496,36 -> 496,43 -> 498,43 -> 498,39 -> 498,43 -> 500,43 -> 500,35 -> 500,43 -> 502,43 -> 502,36 -> 502,43\n480,149 -> 484,149\n482,30 -> 482,23 -> 482,30 -> 484,30 -> 484,20 -> 484,30 -> 486,30 -> 486,26 -> 486,30 -> 488,30 -> 488,23 -> 488,30 -> 490,30 -> 490,20 -> 490,30 -> 492,30 -> 492,25 -> 492,30 -> 494,30 -> 494,29 -> 494,30 -> 496,30 -> 496,24 -> 496,30\n492,125 -> 496,125\n503,70 -> 503,71 -> 518,71\n520,108 -> 520,110 -> 516,110 -> 516,113 -> 529,113 -> 529,110 -> 525,110 -> 525,108\n462,155 -> 462,157 -> 457,157 -> 457,162 -> 471,162 -> 471,157 -> 465,157 -> 465,155\n490,43 -> 490,42 -> 490,43 -> 492,43 -> 492,35 -> 492,43 -> 494,43 -> 494,38 -> 494,43 -> 496,43 -> 496,36 -> 496,43 -> 498,43 -> 498,39 -> 498,43 -> 500,43 -> 500,35 -> 500,43 -> 502,43 -> 502,36 -> 502,43\n462,167 -> 462,168 -> 474,168 -> 474,167\n490,43 -> 490,42 -> 490,43 -> 492,43 -> 492,35 -> 492,43 -> 494,43 -> 494,38 -> 494,43 -> 496,43 -> 496,36 -> 496,43 -> 498,43 -> 498,39 -> 498,43 -> 500,43 -> 500,35 -> 500,43 -> 502,43 -> 502,36 -> 502,43\n514,84 -> 514,79 -> 514,84 -> 516,84 -> 516,78 -> 516,84 -> 518,84 -> 518,77 -> 518,84 -> 520,84 -> 520,82 -> 520,84 -> 522,84 -> 522,79 -> 522,84\n491,17 -> 496,17\n504,68 -> 504,59 -> 504,68 -> 506,68 -> 506,60 -> 506,68 -> 508,68 -> 508,63 -> 508,68 -> 510,68 -> 510,67 -> 510,68\n502,46 -> 502,48 -> 494,48 -> 494,55 -> 507,55 -> 507,48 -> 506,48 -> 506,46\n504,68 -> 504,59 -> 504,68 -> 506,68 -> 506,60 -> 506,68 -> 508,68 -> 508,63 -> 508,68 -> 510,68 -> 510,67 -> 510,68\n504,68 -> 504,59 -> 504,68 -> 506,68 -> 506,60 -> 506,68 -> 508,68 -> 508,63 -> 508,68 -> 510,68 -> 510,67 -> 510,68\n490,43 -> 490,42 -> 490,43 -> 492,43 -> 492,35 -> 492,43 -> 494,43 -> 494,38 -> 494,43 -> 496,43 -> 496,36 -> 496,43 -> 498,43 -> 498,39 -> 498,43 -> 500,43 -> 500,35 -> 500,43 -> 502,43 -> 502,36 -> 502,43\n482,30 -> 482,23 -> 482,30 -> 484,30 -> 484,20 -> 484,30 -> 486,30 -> 486,26 -> 486,30 -> 488,30 -> 488,23 -> 488,30 -> 490,30 -> 490,20 -> 490,30 -> 492,30 -> 492,25 -> 492,30 -> 494,30 -> 494,29 -> 494,30 -> 496,30 -> 496,24 -> 496,30\n504,125 -> 508,125\n462,155 -> 462,157 -> 457,157 -> 457,162 -> 471,162 -> 471,157 -> 465,157 -> 465,155\n490,43 -> 490,42 -> 490,43 -> 492,43 -> 492,35 -> 492,43 -> 494,43 -> 494,38 -> 494,43 -> 496,43 -> 496,36 -> 496,43 -> 498,43 -> 498,39 -> 498,43 -> 500,43 -> 500,35 -> 500,43 -> 502,43 -> 502,36 -> 502,43\n490,43 -> 490,42 -> 490,43 -> 492,43 -> 492,35 -> 492,43 -> 494,43 -> 494,38 -> 494,43 -> 496,43 -> 496,36 -> 496,43 -> 498,43 -> 498,39 -> 498,43 -> 500,43 -> 500,35 -> 500,43 -> 502,43 -> 502,36 -> 502,43\n502,46 -> 502,48 -> 494,48 -> 494,55 -> 507,55 -> 507,48 -> 506,48 -> 506,46\n504,68 -> 504,59 -> 504,68 -> 506,68 -> 506,60 -> 506,68 -> 508,68 -> 508,63 -> 508,68 -> 510,68 -> 510,67 -> 510,68\n486,137 -> 490,137\n502,46 -> 502,48 -> 494,48 -> 494,55 -> 507,55 -> 507,48 -> 506,48 -> 506,46\n462,155 -> 462,157 -> 457,157 -> 457,162 -> 471,162 -> 471,157 -> 465,157 -> 465,155\n529,93 -> 529,95 -> 526,95 -> 526,100 -> 541,100 -> 541,95 -> 533,95 -> 533,93\n504,104 -> 504,105 -> 521,105 -> 521,104\n490,43 -> 490,42 -> 490,43 -> 492,43 -> 492,35 -> 492,43 -> 494,43 -> 494,38 -> 494,43 -> 496,43 -> 496,36 -> 496,43 -> 498,43 -> 498,39 -> 498,43 -> 500,43 -> 500,35 -> 500,43 -> 502,43 -> 502,36 -> 502,43\n471,152 -> 475,152\n520,108 -> 520,110 -> 516,110 -> 516,113 -> 529,113 -> 529,110 -> 525,110 -> 525,108\n514,84 -> 514,79 -> 514,84 -> 516,84 -> 516,78 -> 516,84 -> 518,84 -> 518,77 -> 518,84 -> 520,84 -> 520,82 -> 520,84 -> 522,84 -> 522,79 -> 522,84\n494,15 -> 499,15\n490,43 -> 490,42 -> 490,43 -> 492,43 -> 492,35 -> 492,43 -> 494,43 -> 494,38 -> 494,43 -> 496,43 -> 496,36 -> 496,43 -> 498,43 -> 498,39 -> 498,43 -> 500,43 -> 500,35 -> 500,43 -> 502,43 -> 502,36 -> 502,43\n514,84 -> 514,79 -> 514,84 -> 516,84 -> 516,78 -> 516,84 -> 518,84 -> 518,77 -> 518,84 -> 520,84 -> 520,82 -> 520,84 -> 522,84 -> 522,79 -> 522,84\n492,137 -> 496,137\n480,137 -> 484,137\n520,108 -> 520,110 -> 516,110 -> 516,113 -> 529,113 -> 529,110 -> 525,110 -> 525,108\n482,30 -> 482,23 -> 482,30 -> 484,30 -> 484,20 -> 484,30 -> 486,30 -> 486,26 -> 486,30 -> 488,30 -> 488,23 -> 488,30 -> 490,30 -> 490,20 -> 490,30 -> 492,30 -> 492,25 -> 492,30 -> 494,30 -> 494,29 -> 494,30 -> 496,30 -> 496,24 -> 496,30\n482,30 -> 482,23 -> 482,30 -> 484,30 -> 484,20 -> 484,30 -> 486,30 -> 486,26 -> 486,30 -> 488,30 -> 488,23 -> 488,30 -> 490,30 -> 490,20 -> 490,30 -> 492,30 -> 492,25 -> 492,30 -> 494,30 -> 494,29 -> 494,30 -> 496,30 -> 496,24 -> 496,30\n"