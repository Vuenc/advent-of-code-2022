module Day9 where

import qualified Data.Set as S
import Debug.Trace

inputLines = fmap lines $ readFile "inputs/input9.txt"

-- Star 1

-- pull x = trace ((show x)) (trace ("    -> " ++ (show $ _pull x)) (_pull x))

pull = _pull

_pull ((headX,headY), (tailX,tailY))
    | distX >= 2 && distY == 0  = (headX + relTailDirX, headY)
    | distX == 0 && distY >= 2 = (headX, headY + relTailDirY)
    | distX >= 2 && distY == 1 = (headX + relTailDirX, headY)
    | distX == 1 && distY >= 2 = (headX, headY + relTailDirY)
    | distX >= 2 && distY >= 2 && distX >= distY = (tailX - relTailDirX, tailY - relTailDirY)
    | distX >= 2 && distY >= 2 && distX < distY = (tailX - relTailDirX, tailY - relTailDirY)
    | distX <= 1 && distY <= 1 = (tailX, tailY)
    | otherwise = undefined -- f ((headX,headY),(tailX,tailY)) 
    where distX = abs (headX - tailX) :: Int
          distY = abs (headY - tailY) :: Int
          relTailDirX = if tailX > headX then 1 else -1
          relTailDirY = if tailY > headY then 1 else -1


f ((headX,headY),(tailX,tailY)) = (tailX, tailY)

step (dX,dY) ((headX,headY), tailXY, visitedPositions) =
    let newHead = (headX+dX, headY+dY)
        newTail = pull (newHead, tailXY)
    in (newHead, newTail, S.insert newTail visitedPositions)

simulateInstruction  (headXYtailXY) (dir:' ':repeats) =
    let dXdY = if dir == 'R' then (1,0) else if dir == 'L' then (-1,0) else if dir == 'U' then (0,-1) else if dir == 'D' then (0,1) else undefined
    in (iterate (step dXdY) headXYtailXY) !! (read repeats)

computeSolution1 inputLines = length visitedPositions
    where initialState = ((0,0), (0,0), S.empty)
          (_, _, visitedPositions) = foldl simulateInstruction initialState inputLines


-- Star 2



stepManyKnots (dX,dY) ((headX,headY):knotPositions, visitedPositions) =
    let newHead = (headX+dX, headY+dY)
        newKnotPositions = map pull $ zip (newHead:newKnotPositions) (knotPositions)
        newTail = last newKnotPositions
    in (newHead:newKnotPositions, S.insert newTail visitedPositions)

simulateInstructionManyKnots  knotPositions (dir:' ':repeats) =
    let dXdY = if dir == 'R' then (1,0) else if dir == 'L' then (-1,0) else if dir == 'U' then (0,-1) else if dir == 'D' then (0,1) else undefined
    in (iterate (stepManyKnots dXdY) knotPositions) !! (read repeats)


computeSolution2 inputLines = length visitedPositions
    where initialState = (take 10 $ repeat (0,0), S.empty)
          (_, visitedPositions) = foldl simulateInstructionManyKnots initialState inputLines


l = lines "R 5\nU 8\nL 8\nD 3\nR 17\nD 10\nL 25\nU 20"

-- Solutions (IO)
solution1 = fmap computeSolution1 inputLines
solution2 = fmap computeSolution2 inputLines
