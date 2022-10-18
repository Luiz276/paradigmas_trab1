-- Trabalho 1 - Structure - Kojun Solver

module Structure where

import Control.Applicative ((<|>))
import qualified Control.Monad
import qualified Data.Char
import qualified Data.Function
import qualified Data.List.Split
import qualified Data.List

-- 1) TYPES
-- value is either fixed with a particular digit or has a set of digits as possibilities
data Value = Fixed Int | Possible [Int] deriving (Show, Eq)
-- row is a list of values
type Row = [Value]
-- grid is a list of rows
type Grid = [Row]
-- puzzle is a list of grids
type Puzzle = [Grid]

------------------------------------------------------------------------------------------

-- 2) BASIC STRUCTURE FUNCTIONS ...(for rows, grids, values, etc)

-- dimension of the puzzle
dimension :: Puzzle -> Int
dimension puzzle = length (puzzle !! 0)

-- split the groups in the puzzle
splitGroups :: Eq m => Puzzle -> Grid -> Puzzle
splitGroups value groups = [filterGroup group tupleValues | group <- mapGroups]
  where
    tupleValues = foldl1 (++) (zipWith zip values groups)
    mapGroups = nub (map snd tupleValues)
    filterGroup group list = map fst $ filter ((== group) . snd) list

-- list of values in a group
listValues :: Eq m => Puzzle -> Grid -> Value -> [m]
listValues value group id = map fst $ filter ((==id) . snd) tupleValues 
  where
    tupleValues = foldl1 (++) (zipWith zip value group)

groupRow :: Eq m => Puzzle -> Grid -> [Row]
groupRow value group = zipWith zip (Row value) (Row group) >>= map (map fst) . groupBy (\m b -> snd m == snd b)

-- size of a group
sizeGroup :: Eq m => m -> Puzzle -> Int
sizeGroup _ [] = 0
sizeGroup id group = sum [count id p | p <- group]
  where count x xs = length (filter (==x) xs)

-- rebuild the original columns from the list of columns divided by blocks
originalRow :: [Row] -> Int -> [Row]
originalRow bs n = subList n (concat bs)

subList :: Int -> [l] -> [[l]]
subList n = takeWhile (not . null) . map (take n) . iterate (drop n)
