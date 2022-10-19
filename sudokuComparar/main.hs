import Control.Monad.STM (check)
import Sudoku

main = do
  let grid =
        [ [3, 1, 6, 5, 1, 8, 4, 9, 9],
          [5, 2, 1, 1, 0, 0, 0, 0, 0],
          [0, 8, 7, 0, 0, 0, 0, 3, 1],
          [0, 0, 3, 0, 1, 0, 0, 8, 0],
          [9, 0, 0, 8, 6, 3, 0, 0, 5],
          [0, 5, 0, 0, 9, 0, 6, 0, 0],
          [1, 3, 0, 0, 0, 0, 2, 5, 0],
          [0, 0, 0, 0, 0, 0, 0, 7, 4],
          [0, 0, 5, 2, 0, 6, 3, 0, 0]
        ]

  --   let first = [3, 1, 6, 5, 1, 8, 4, 9, 9]
  let grid = [[0 | i <- [1 .. 9]] | j <- [1 .. 9]]
  let result = solveSudoku grid
  print result