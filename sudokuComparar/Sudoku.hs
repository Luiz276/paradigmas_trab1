module Sudoku where

import Data.List
import Distribution.Simple.InstallDirs (InstallDirs (docdir))

-- printar o resultado encontrado
printRow :: [Int] -> [Char] -> IO ()
printRow [] _ = do
  putStrLn ""
printRow (c : d) [] = do
  putStr " "
  putStr (show c)
  printRow d []
printRow (c : d) (e : f) = do
  putStr " "
  putStr (show c)
  putStr " "
  putStr (show e)
  printRow d f

printSudoku :: [[Int]] -> ([[Char]], [[Char]]) -> IO ()
printSudoku [] _ = putStrLn ""
printSudoku (a : b) ((crh : crb), []) = do
  printRow a crh
  printSudoku b (crb, [])
printSudoku (a : b) ((crh : crb), (cvh : cvb)) = do
  printRow a crh
  printCompVer cvh
  printSudoku b (crb, cvb)
  where
    printCompVer [] = putStrLn ""
    printCompVer (g : h) = do
      putStr (show g)
      putStr "   "
      printCompVer h

-- funcao para chamar no inicio
solveSudoku :: [[Int]] -> ([[Char]], [[Char]]) -> (Bool, [[Int]])
solveSudoku grid comps
  | isSolved grid == (-1, -1) = (True, grid)
  | otherwise = try grid comps pos 1
  where
    pos = isSolved grid

-- funcao para testar todas as possibilidades na posição
try :: [[Int]] -> ([[Char]], [[Char]]) -> (Int, Int) -> Int -> (Bool, [[Int]])
try grid comps pos num
  | num > 9 = (False, grid)
  | otherwise = tryAs grid comps pos num

tryAs :: [[Int]] -> ([[Char]], [[Char]]) -> (Int, Int) -> Int -> (Bool, [[Int]])
tryAs grid comps pos num
  | validity && solved = (True, gridfinal)
  | otherwise = try grid comps pos num2
  where
    validity = test grid comps pos num
    grid2 = changeCell grid pos num
    (solved, gridfinal) = solveSudoku grid2 comps
    num2 = num + 1

-- testa se posição é aceita
test :: [[Int]] -> ([[Char]], [[Char]]) -> (Int, Int) -> Int -> Bool
test grid char pos num = and [checkColumn grid pos num, checkLine grid pos num, checkSquare grid pos num, checkComp grid char pos num]

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

-- testa comparações
checkComp :: [[Int]] -> ([[Char]], [[Char]]) -> (Int, Int) -> Int -> Bool
checkComp grid (comp_hor, comp_ver) (row, column) num = and $ map compare neighboors
  where
    neighboors = getNeighboors grid (row, column)
    compare :: (Int, Int) -> Bool
    compare (n_row, n_column) = finalCompare n_num num operator
      where
        n_num = grid !! n_row !! n_column
        operator = getOperator (row, column) (n_row, n_column) (comp_hor, comp_ver)
        finalCompare l r op
          | op == '<' = l < r
          | otherwise = l > r

-- pega operador entre as posicoes
getOperator :: (Int, Int) -> (Int, Int) -> ([[Char]], [[Char]]) -> Char
getOperator (row, column) (n_row, n_column) (comp_hor, comp_ver)
  | row == n_row = getop (row, column) (n_row, n_column) comp_hor
  | otherwise = getop (row, column) (n_row, n_column) comp_ver
  where
    getop (r, c) (nr, nc) comps = comps !! minr !! minc
      where
        minr = getMin r nr
        minc = getMin c nc
        getMin a b
          | a < b = a
          | otherwise = b

-- pega vizinhos de posição
getNeighboors :: [[Int]] -> (Int, Int) -> [(Int, Int)]
getNeighboors grid (row, column)
  | relativeRow >= 0 && relativeColumn >= 0 = [column_n, row_n]
  | relativeColumn >= 0 = [row_n]
  | relativeRow >= 0 = [column_n]
  | otherwise = []
  where
    relativeRow = (row `mod` 3) - 1
    relativeColumn = (column `mod` 3) - 1
    column_n = (row -1, column)
    row_n = (row, column -1)

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