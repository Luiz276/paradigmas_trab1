module Sudoku.Matrix where

data Casa = Fixed Int | Possible [Int] deriving (Show, Eq)
type Linha = [Casa]
type Grid = [Linha]


readInput :: String -> Grid
readInput s
  | length s == 81 = traverse (traverse readCell) . Data.List.Split.chunksOf 9 $ s
  | otherwise      = Nothing
  where
    readCell '.' = Just $ Possible [1..9]
    readCell c
      | Data.Char.isDigit c && c > '0' = Just . Fixed . Data.Char.digitToInt $ c
      | otherwise = Nothing

showGrid :: Grid -> String
showGrid = unlines . map (unwords . map showCasa)

--main = do
--    print(showGrid )