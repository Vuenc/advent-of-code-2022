module Day12 where

-- import Control.Monad.Applicative
-- import Control.Monad.Search
-- import Data.Monoid (Sum(..))
import qualified Data.Map as M
import Algorithm.Search (dijkstra)
import Data.Char (ord)
import Data.Foldable (find)
import Debug.Trace (trace)
import Data.Function (on)

inputLines = fmap lines $ readFile "inputs/input12.txt"

data Cell = Cell {
    coordinates :: (Int, Int),
    char :: Char,
    field :: [[Cell]]
}

instance Show Cell where
    show cell = "[(" ++ (show i) ++ ", " ++ (show j) ++ "): " ++ [(char cell)] ++ "]"
        where (i,j) = coordinates cell

instance Eq Cell where
    (==) = (==) `on` coordinates

instance Ord Cell where
    (<=) = (<=) `on` coordinates

-- Star 1

-- was trying to understand https://hackage.haskell.org/package/monad-dijkstra-0.1.1.3/docs/Control-Monad-Search.html
-- naturals :: Search (Sum Integer) Integer
-- naturals = return 0 <|> (cost' (Sum 1) >> ((+ 2) <$> naturals))

-- -- All pairs of naturals
-- pairs :: Search (Sum Integer) (Integer, Integer)
-- pairs = (,) <$> naturals <*> naturals

-- elevationChar cell = char $ ((field cell) !! j) !! i
--     where (i,j) = coordinates cell

_elevation 'S' = ord 'a'
_elevation 'E' = ord 'z'
_elevation x = ord x

elevation = _elevation . char

potentialNeighborCells cell = map (\(i,j) -> ((field cell) !! j) !! i) potentialNeighborCoords
        where (x,y) = coordinates cell
              potentialNeighborCoords = [(i,j) | (i,j) <- [(x+1,y),(x-1,y),(x,y+1),(x,y-1)], i >= 0, i < length (head $ field cell), j >= 0, j < length (field cell)]


neighbors cell = filter isPassable (potentialNeighborCells cell)
    where isPassable otherCell = (elevation otherCell) <= (elevation cell) + 1

computeField inputLines = field
    where field = [[Cell {field = field, char = c, coordinates = (i,j)} | (i, c) <- zip [0..] row] | (j, row) <- zip [0..] inputLines]

computeSolution1 inputLines = cost
    where field = computeField inputLines
          Just startCell = find ((=='S') . char) $ concat field
          Just (cost, _) = dijkstra neighbors transitionCost solutionFound startCell
          transitionCost i j = 1
          solutionFound cell = (char cell) == 'E'

l = lines "Sabqponm\nabcryxxl\naccszExk\nacctuvwj\nabdefghi"

-- Star 2

neighborsReverse cell = filter isPassable (potentialNeighborCells cell)
    where isPassable otherCell = (elevation cell) <= (elevation otherCell) + 1

computeSolution2 inputLines = cost
    where field = computeField inputLines
          Just startCell = find ((=='E') . char) $ concat field
          Just (cost, _) = dijkstra neighborsReverse transitionCost solutionFound startCell
          transitionCost i j = 1
          solutionFound cell = (char cell) == 'a' || (char cell) == 'S'

-- Solutions (IO)
solution1 = fmap computeSolution1 inputLines
solution2 = fmap computeSolution2 inputLines
