module Day22 where

-- import Data.List.Split (splitOn)
import qualified Data.Map as M
import Data.List.Split (splitOn)
import Data.List.Utils (replace)
import Data.Foldable (minimumBy)
import Data.Function (on)
import Debug.Trace (trace, traceShow)
import Data.List (nub, intercalate)
import Data.Maybe (fromJust)
-- import Data.Text (replace)

inputLines = fmap lines $ readFile "inputs/input22.txt"

buildField :: [[Char]] -> M.Map (Int, Int) Char
buildField fieldLines = M.fromList $ concat $ [[((x,y),char) | (x, char) <- zip [0..] line, char /= ' '] | (y, line) <- zip [0..] fieldLines]

move field (x,y) _ 0 = (x,y)
move field (x,y) (dx, dy) k =
    let newxy = (x+dx, y+dy)
        outxy = case M.lookup newxy field of
            Just '.' -> newxy
            Just '#' -> (x,y)
            Nothing -> 
                let wrappedxy = last $ takeWhile (flip M.member $ field) [(x-k*dx, y-k*dy) | k <- [1..]]
                in case M.lookup wrappedxy field of 
                    Just '.' -> wrappedxy
                    Just '#' -> (x,y)
    in if outxy == (x,y) then outxy else move field outxy (dx,dy) (k-1)

-- rotate (1, 0) 'R' = (0, 1)
-- rotate (0, 1) 'R' = (-1, 0)
-- rotate (-1, 0) 'R' = (0, -1)
-- rotate (0, -1) 'R' = (1, 0)
-- rotate (1, 0) 'L' = (0, -1)
-- rotate (0, 1) 'L' = (1, 0)
-- rotate (-1, 0) 'L' = (0, 1)
-- rotate (0, -1) 'L' = (-1, 0)
rotate (dx, dy) 'R' = (-dy, dx)
rotate (dx, dy) 'L' = (dy, -dx)

-- Star 1

computeSolution1 inputLines =
    let [fieldLines, [keyLine]] = splitOn [""] inputLines
        field = buildField fieldLines
        instrs = zip [1..] $ splitOn " " $ replace "R" " R " $ replace "L" " L " keyLine
        applyInstr ((x,y),(dx,dy)) "R" = ((x,y),(rotate (dx,dy) 'R'))
        applyInstr ((x,y),(dx,dy)) "L" = ((x,y),(rotate (dx,dy) 'L'))
        applyInstr ((x,y),(dx,dy)) num = (move field (x,y) (dx,dy) (read num), (dx,dy))
        initialXY = minimumBy (compare `on` (\(x,y) -> (y,x))) $ M.keys field
        ((finalx, finaly), (finaldx, finaldy)) = foldl (\st (i,inp) -> trace (show i ++ " " ++ show st) $ applyInstr st inp) (initialXY, (1,0)) instrs
        directionScore = case (finaldx, finaldy) of
            (1,0) -> 0
            (0, 1) -> 1
            (-1, 0) -> 2
            (0, -1) -> 3
    in directionScore + 1000 * (finaly + 1) + 4 * (finalx + 1)

-- Star 2

-- consider the cube 
-- T        (top)
-- F-R-O-L  (front-right-oppisite-left)
-- B        (back)
data Side = T | F | R | O | L | B deriving (Eq, Show, Ord)

data Direction = E | S | W | N | E' | S' | W' | N' deriving (Eq, Show, Ord)

fromSideIndex E = 0
fromSideIndex S = 1
fromSideIndex W = 2
fromSideIndex N = 3


-- ok let's NOT generalize, I'll just take it specific to my input
--   T-R
--   F
-- L-B
-- O

-- adjacent in the following sense:
--   4
-- 3 X 1
--   2
-- adjacent T = [R, F, L, O]
-- adjacent F = [R, B, L, T]
-- adjacent R = [O, B, F, T]
-- adjacent O = [L, B, ]
adjacent T = [(R,W), (F,N), (L, W'), (O, W)]
adjacent F = [(R, S), (B, N), (L, N), (T, S)]
adjacent R = [(B, E'), (F, E), (T, E), (O, S)]
adjacent O = [(B, S), (R, N), (T, N), (L, S)]
adjacent L = [(B, W), (O, N), (T, W'), (F, W)]
adjacent B = [(R, E'), (O, E), (L, E), (F, S)]

coordToFace = M.fromList [((1,0), T), ((2,0), R), ((1,1), F), ((0,2), L), ((1,2), B), ((0,3), O)]

faceToCoord = M.fromList $ [(face, coord) | (coord, face) <- M.toList coordToFace]

toCubeFace x y  = M.lookup (x `div` 50, y `div` 50) coordToFace

-- overstepSideRelativeCoords (x,_) S N = (x, 0)
overstepSideRelativeCoords (x,y) E S = (y, 49)
overstepSideRelativeCoords (x,y) E W = (0, y)
overstepSideRelativeCoords (x,y) E E' = (49, 49-y)
overstepSideRelativeCoords (x,y) S E = (49, x)
overstepSideRelativeCoords (x,y) S N = (x, 0)
overstepSideRelativeCoords (x,y) W E = (49, y)
overstepSideRelativeCoords (x,y) W N = (y, 0)
overstepSideRelativeCoords (x,y) W W' = (0, 49-y)
overstepSideRelativeCoords (x,y) N S = (x, 49)
overstepSideRelativeCoords (x,y) N W = (0, x)
overstepSideRelativeCoords (x,y) N W' = (0, 49-x)

overstepSideDirection (dx,dy) fromSide toSide =
    let _numRRotations E S = 3
        _numRRotations E W = 0
        _numRRotations E E' = 2
        _numRRotations S E = 1
        _numRRotations S N = 0
        _numRRotations W E = 0
        _numRRotations W N = 3
        _numRRotations W W' = 2
        _numRRotations N S = 0
        _numRRotations N W = 1
        _numRRotations N W' = 3
    in iterate (flip rotate 'R') (dx,dy) !! (_numRRotations fromSide toSide) 

-- overstepSideRelativeCoords (x,y) originSide targetSide = undefined

directionToChar (1,0) = '>'
directionToChar (0,1) = 'v'
directionToChar (-1,0) = '<'
directionToChar (0,-1) = '^'

move' field (x,y) (dx,dy) 0 visited = ((x,y),(dx,dy),visited)
move' field (x,y) (dx, dy) k visited =
    let newxy@(newx,newy) = (x+dx, y+dy)
        ((outxy),(outdxy)) = case M.lookup newxy field of
            Just '.' -> (newxy, (dx,dy))
            Just '#' -> ((x,y),(dx,dy))
            Nothing -> 
                let (fieldBaseX, fieldBaseY) = (x - x `mod` 50, y - y `mod` 50)
                    (relX, relY) = (newx-fieldBaseX, newy-fieldBaseY)
                    currentFace = fromJust $ toCubeFace x y
                    fromSide 
                        | relX < 0 = W
                        | relX >= 50 = E
                        | relY < 0 = N
                        | relY >= 50 = S
                    (toFace, toSide) = adjacent currentFace !! (fromSideIndex fromSide)
                    (newRelX, newRelY) = overstepSideRelativeCoords (relX, relY) fromSide toSide
                    (toFaceX, toFaceY) = (M.!) faceToCoord toFace
                    newAbsXY = (toFaceX * 50 + newRelX, toFaceY * 50 + newRelY)
                    newdXY = overstepSideDirection (dx, dy) fromSide toSide
                in case M.lookup newAbsXY field of
                        Just '.' -> (newAbsXY, newdXY)
                        Just '#' -> ((x,y),(dx,dy))
                        Nothing -> trace ((show (newAbsXY, fromSide, toSide, currentFace, toFace, (toFaceX, toFaceY), (relX, relY), (newRelX, newRelY), (toFaceX, toFaceY)))) undefined
                -- let wrappedxy = last $ takeWhile (flip M.member $ field) [(x-k*dx, y-k*dy) | k <- [1..]]
                -- in case M.lookup wrappedxy field of 
                --     Just '.' -> wrappedxy
                --     Just '#' -> (x,y)
        result = if outxy == (x,y) then (outxy, outdxy, visited) else move' field outxy outdxy (k-1) (M.insert outxy (directionToChar outdxy) visited)
    in result
    -- in trace (show result) result

-- Star 1

showField field visited =
    intercalate "\n" [[if M.member (x,y) visited then (M.!) visited (x,y) else if M.member (x,y) field then (M.!) field (x,y) else ' ' | x <- [0..150]] | y <- [0..200]]
    
computeSolution2 inputLines =
    let [fieldLines, [keyLine]] = splitOn [""] inputLines
        fieldLines' = map (replace "#" ".") fieldLines
        field = buildField fieldLines
        instrs = zip [1..] $ splitOn " " $ replace "R" " R " $ replace "L" " L " keyLine
        applyInstr ((x,y),(dx,dy),visited) "R" = ((x,y),(rotate (dx,dy) 'R'),visited)
        applyInstr ((x,y),(dx,dy),visited) "L" = ((x,y),(rotate (dx,dy) 'L'),visited)
        applyInstr ((x,y),(dx,dy),visited) num = move' field (x,y) (dx,dy) (read num) visited
        initialXY = minimumBy (compare `on` (\(x,y) -> (y,x))) $ M.keys field
        ((finalx, finaly), (finaldx, finaldy), visited) = traceShow (map snd instrs) $ foldl (\st (i,inp) -> applyInstr st inp) (initialXY, (1,0), M.empty) instrs
        directionScore = case (finaldx, finaldy) of
            (1,0) -> 0
            (0, 1) -> 1
            (-1, 0) -> 2
            (0, -1) -> 3
    in trace (showField field visited) $ directionScore + 1000 * (finaly + 1) + 4 * (finalx + 1)


-- Solutions (IO)
solution1 = fmap computeSolution1 inputLines
solution2 = fmap computeSolution2 inputLines
