module Sudoku.Solver where

-- Tratando os digitos das casas -> se sao pontos ou valores fixados
valuesCell :: [Cell] -> [Cell]
valuesCell cells = traverse valuesCell cells
    where
        fixeds = [x | Fixed x <- cells]
valuesCell (Possible xs) = case xs Data.List.\\ fixeds of
      []  -> Nothing
      [y] -> Just $ Fixed y
      ys  -> Just $ Possible ys
    valuesCell x = Just x

-- Tratando os grids
transformGrid :: Grid -> Grid
transformGrid =
  concatMap (\rows -> let [r1, r2, r3] = map (Data.List.Split.chunksOf 3) rows
                      in zipWith3 (\a b c -> a ++ b ++ c) r1 r2 r3)
  . Data.List.Split.chunksOf 3

-- Usando as funcoes acima para tratar o grid de input
newGrid :: Grid -> Grid
newGrid grid =
  traverse valuesCells grid
  >>= fmap Data.List.transpose . traverse valuesCells . Data.List.transpose
  >>= fmap transformGrid . traverse valuesCells . transformGrid

fixingGrid :: Grid -> Grid
fixingGrid = fixM NewGrid
  where
    fixM f x = f x >>= \x' -> if x' == x then return x else fixM f x'