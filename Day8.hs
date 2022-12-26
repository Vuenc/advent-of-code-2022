module Day8 where
import Data.Array (Array)

inputLines = fmap lines $ readFile "inputs/input8.txt"

-- Star 1

type Array2D = [[Char]]

-- rotate90 :: Array2D -> Array2D
rotate90 x = _rotate90 x [] [[]]

-- rotate270 = rotate90 . rotate90 . rotate90

-- _rotate90 (l_ls:[]) lss2 
_rotate90 ((head:restLine):otherLines) (aboveLines) (current90Line:above90Lines) = _rotate90 otherLines (restLine:aboveLines) ((current90Line ++ [head]):above90Lines)
_rotate90 [] aboveLines@((_:_):_) current90Lines = _rotate90 (reverse aboveLines) [] ([]:current90Lines)
_rotate90 [] ([]:_) current90Lines = current90Lines

l = lines "30373\n25512\n65332\n33549\n35390"

visibilitiesLine maxHeight (l:ls) = (currentVisibility):(visibilitiesLine newMaxHeight ls)
    where height = read [l]
          newMaxHeight = max maxHeight height
          currentVisibility = height > maxHeight
visibilitiesLine _ [] = []

visibilities grid = map (visibilitiesLine (-1)) grid

showGrid grid = mapM_ print grid

computeSolution1 inputLines = out (flattenedVisibilities !! 0) (flattenedVisibilities !! 1) (flattenedVisibilities !! 2) (flattenedVisibilities !! 3)
    where rotatedGrids = take 4 $ (iterate rotate90) inputLines
          gridVisibilities = map visibilities rotatedGrids
          backRotatedVisibilites = [((iterate rotate90) gridVis) !! times | (gridVis, times) <- zip gridVisibilities [0,3,2,1]]
          flattenedVisibilities = map concat backRotatedVisibilites
          out (l1:ls1) (l2:ls2) (l3:ls3) (l4:ls4) = (if or [l1,l2,l3,l4] then 1 else 0) + (out ls1 ls2 ls3 ls4)
          out [] [] [] [] = 0


-- Star 2


viewingDistances ls = map (_viewingDistancesByLine $ repeat 0) ls


_viewingDistancesByLine viewingDistanceByHeight (l:ls) = (viewingDistanceByHeight !! h):(_viewingDistancesByLine newViewingDistanceByHeight  ls)
    where h = read [l]
          newViewingDistanceByHeight = map (\(i,dist) -> if i <= h then 1 else dist + 1) (zip [0..] viewingDistanceByHeight)
_viewingDistancesByLine _ [] = []

computeSolution2 inputLines = maximum $ scenicScores (flattenedViewingDistances !! 0) (flattenedViewingDistances !! 1) (flattenedViewingDistances !! 2) (flattenedViewingDistances !! 3)
    where rotatedGrids = take 4 $ (iterate rotate90) inputLines
          gridViewingDistances = map viewingDistances rotatedGrids
          backRotatedViewingDistances = [((iterate rotate90) gridViewDist) !! times | (gridViewDist, times) <- zip gridViewingDistances [0,3,2,1]]
          flattenedViewingDistances = map concat backRotatedViewingDistances
          scenicScores (l1:ls1) (l2:ls2) (l3:ls3) (l4:ls4) = (l1*l2*l3*l4):(scenicScores ls1 ls2 ls3 ls4)
          scenicScores [] [] [] [] = []



-- Solutions (IO)
solution1 = fmap computeSolution1 inputLines
solution2 = fmap computeSolution2 inputLines
