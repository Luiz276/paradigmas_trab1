module Sudoku where

import Data.List
import Distribution.Parsec.Position (positionCol)

getX :: (Int, Int) -> Int
getX (x, _) = x

getY :: (Int, Int) -> Int
getY (_, y) = y

getValidity :: (Bool, [[Int]]) -> Bool
getValidity (value, _) = value

-- funcao para chamar no inicio
solveSudoku :: [[Int]] -> (Bool, [[Int]])
solveSudoku grid
  | isSolved grid == (-1, -1) = (True, grid)
  | otherwise = try grid pos 1
  where
    pos = isSolved grid

-- funcao para testar todas as possibilidades na posição
try :: [[Int]] -> (Int, Int) -> Int -> (Bool, [[Int]])
try grid pos num
  | num > 9 = (False, grid)
  | otherwise = tryAs grid pos num

tryAs :: [[Int]] -> (Int, Int) -> Int -> (Bool, [[Int]])
tryAs grid pos num
  | validity && solved = (True, gridfinal)
  | otherwise = try grid pos num2
  where
    validity = test grid pos num
    grid2 = changeCell grid pos num
    (solved, gridfinal) = solveSudoku grid2
    num2 = num + 1

-- testa se posição é aceita
test :: [[Int]] -> (Int, Int) -> Int -> Bool
test grid pos num = and [checkColumn grid pos num, checkLine grid pos num, checkSquare grid pos num]

-- testa linha
checkLine :: [[Int]] -> (Int, Int) -> Int -> Bool
checkLine grid (row, column) num = checkLineAs gridRow num
  where
    gridRow = getRow grid row

checkLineAs :: [Int] -> Int -> Bool
checkLineAs [] _ = True
checkLineAs (a : b) num
  | a == num = False
  | otherwise = checkLineAs b num

-- testa coluna
checkColumn :: [[Int]] -> (Int, Int) -> Int -> Bool
checkColumn grid (row, column) num = checkLineAs listColumn num
  where
    listColumn = getListColumn grid column

getListColumn :: [[Int]] -> Int -> [Int]
getListColumn grid column = map (head . drop column) grid

-- testa quadrado
checkSquare :: [[Int]] -> (Int, Int) -> Int -> Bool
checkSquare grid (row, column) num = checkLineAs listSquare num
  where
    coordinates = getSquareCoods (row, column)
    listSquare = getListSquare grid coordinates

getSquareCoods :: (Int, Int) -> (Int, Int)
getSquareCoods (row, column) = (row - (row `mod` 3), column - (column `mod` 3))

getListSquare :: [[Int]] -> (Int, Int) -> [Int]
getListSquare grid (row, column) = sliceSquare row ++ sliceSquare row2 ++ sliceSquare row3
  where
    row2 = row + 1
    row3 = row + 2
    sliceSquare a = slice column column2 (getRow grid a)
      where
        column2 = column + 2

slice :: Int -> Int -> [a] -> [a]
slice from to xs = take (to - from + 1) (drop from xs)

isSolved :: [[Int]] -> (Int, Int) -- (row, column)
isSolved grid = getPos columns 0
  where
    columns = map getColumn grid

-- Pega uma lista de int e retorna a primeira posição = 0
-- caso todas sejam zero retorna -1
getColumn :: [Int] -> Int
getColumn row = getColumnAs row 0

getColumnAs :: [Int] -> Int -> Int
getColumnAs [] cnt = -1
getColumnAs (a : b) cnt
  | a == 0 = cnt
  | otherwise = getColumnAs b cnt2
  where
    cnt2 = cnt + 1

-- mesma coisa que get column mas devolve uma tupla
getPos :: [Int] -> Int -> (Int, Int)
getPos (a : b) cnt
  | a /= -1 = (cnt, a)
  | otherwise = getPos b cnt2
  where
    cnt2 = cnt + 1
getPos [] _ = (-1, -1)

-- testa se é possível completar o sudoku com esse numero
-- test :: [[Int]] -> (Int, Int) -> Int -> (Bool, [[Int]])
-- novo grid com o valor da coordenada mudado
changeCell :: [[Int]] -> (Int, Int) -> Int -> [[Int]]
changeCell grid (row, column) num = before ++ [newrow] ++ after
  where
    before = take row grid
    after = drop row2 grid
      where
        row2 = row + 1
    newrow = beforeRow ++ [num] ++ afterRow
      where
        beforeRow = take column gridRow
          where
            gridRow = getRow grid row
        afterRow = drop column2 gridRow
          where
            column2 = column + 1
            gridRow = getRow grid row

getRow :: [[Int]] -> Int -> [Int]
getRow grid index = getRowAs grid index 0

getRowAs :: [[Int]] -> Int -> Int -> [Int]
getRowAs [] _ _ = []
getRowAs (a : b) index cnt
  | cnt == index = a
  | otherwise = getRowAs b index cnt2
  where
    cnt2 = cnt + 1