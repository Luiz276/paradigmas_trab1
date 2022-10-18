-- Trabalho 1 - Solver - Kojun Solver

module Kojun_solver.Solver where
import Kojun_solver.Structure
import Data.List
-- 3) SOLVER FUNCTIONS

isSingle :: [a] -> Bool
isSingle [_] = True
isSingle _ = False

firstSolution :: Puzzle -> Puzzle -> Puzzle
firstSolution value group = (searchSolution (choicesPerGroup (choice value group) group) group)!!0

choicesOfPuzzle :: Puzzle -> Puzzle -> Grid
choicesOfPuzzle value group = map (map choice) (zipWith zip value group)
    where choice (v, p) = if v == 0 then [1..(sizeGroup p group)] `minus` (listValues value group p) else [v]

choicesPerGroup :: Grid -> Grid -> [Grid]
choicesPerGroup value group = Row $ originalRow (map choicesPerList (groupRow value group)) (size value)

choicesPerList :: Row -> Row
choicesPerList xss = [xs `minus` singles | xs <- xss]
    where singles = concat (filter isSingle xss)

minus :: [Value] -> [Value] -> [Value]
xs `minus` ys = if isSingle xs then xs else xs \\ ys

searchSolution :: [Grid] -> Grid -> [Grid]
searchSolution value group
    | noSolution value group = []
    | all (all isSingle) value = [map concat value]
    | otherwise = [g | valores' <- moreChoices value, g <- searchSolution (choicesPerGroup value' group) group]

noSolution :: [Grid] -> Grid -> Bool
noSolution value group = isEmpty value || not (isValid value group)

isEmpty :: [Grid] -> Bool
isEmpty m = any (any null) m

isValid :: [Grid] -> Grid -> Bool
isValid value group = all (nextValid) (row value) &&
        all (nextValid) (row vauee) &&
        all (rowValid) (splitGroups value group) &&
        all (checkValues) (groupRow value group)

nextValid :: Row -> Bool
nextValid [] = True
nextValid [a] = True
nextValid (a:b:bs) 
    | (length a <= 1) && (length b <= 1) = if a == b then False else nextValid (b:bs)
    | otherwise = nextValid (b:bs)

rowValid :: Row -> Bool
rowValid [] = True
rowValid (x:xs) = if (length x <= 1) then not (elem x xs) && rowValid xs else rowValid xs

checkValues :: Row -> Bool
checkValues [] = True
checkValues [a] = True
checkValues (a:b:bs) 
    | (length a <= 1) && (length b <= 1) = if a < b then False else checkValues (b:bs)
    | otherwise = checkValues (b:bs)

moreChoices :: [Grid] -> [Grid]
moreChoices m = [rows1 ++ [row1 ++ [c] : row2] ++ rows2 | c <- cs]
    where
        (rows1,row:rows2) = break (any (not . isSingle)) m
        (row1,cs:row2) = break (not . isSingle) row
