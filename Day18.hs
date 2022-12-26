module Day18 where

import Data.List.Split (splitOn)
import qualified Data.Set as S
import Data.Graph (graphFromEdges, reachable)
import Data.Maybe (fromJust)

inputLines = fmap lines $ readFile "inputs/input18.txt"

-- Star 1


inputPoints :: [[Char]] -> [[Int]]
inputPoints inputLines = map readPoint inputLines
    where readPoint line = map read $ splitOn "," line

adjacentCoords [x,y,z] = [[x+1,y,z], [x-1,y,z], [x,y+1,z], [x,y-1,z], [x,y,z+1], [x,y,z-1]]

surfaceAreaContribution cubes coords = sum $ map (fromEnum . not . ((flip S.member) cubes)) $ adjacentCoords coords

computeSolution1 inputLines = sum $ map (surfaceAreaContribution cubes) (S.toList cubes)
    where cubes = S.fromList $ inputPoints inputLines

l = lines "2,2,2\n1,2,2\n3,2,2\n2,1,2\n2,3,2\n2,2,1\n2,2,3\n2,2,4\n2,2,6\n1,2,5\n3,2,5\n2,1,5\n2,3,5"

-- Star 2

-- surfaceAreaContribution cubes coords = sum $ map (fromEnum . not . ((flip S.member) cubes)) $ adjacentCoords coords

computeSolution2 inputLines =
    let cubes = inputPoints inputLines
        cubesSet = S.fromList cubes
        -- pointList = S.toList points
        (xs, ys, zs) = (fmap (!!0) cubes, fmap (!!1) cubes, fmap (!!2) cubes)
        airCubes = [[x,y,z] | x <- [minimum xs - 1 .. maximum xs + 1], y <- [minimum ys - 1 .. maximum ys + 1], z <- [minimum zs - 1 .. maximum zs + 1], not $ S.member [x,y,z] cubesSet]
        airCubesSet = S.fromList airCubes
        adjacentAirCoords coords = filter (not . ((flip S.member) cubesSet)) $ adjacentCoords coords
        (emptyCellsGraph, vertexToKey, keyToVertex) = graphFromEdges [(coords, coords, adjacentAirCoords coords) | coords <- airCubes]
        outsideCoord = [minimum xs - 1, minimum ys - 1, minimum zs - 1]
        enclosedAirCubes = S.difference airCubesSet $ (S.fromList $ map (\v -> let (k, _, _) = vertexToKey v in k) $ reachable emptyCellsGraph (fromJust $ keyToVertex outsideCoord))
    in sum $ map (surfaceAreaContribution (S.union cubesSet enclosedAirCubes)) cubes

-- Solutions (IO)
solution1 = fmap computeSolution1 inputLines
solution2 = fmap computeSolution2 inputLines
