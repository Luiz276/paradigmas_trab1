module Kojun_2.Input where

-- First example, a 8x8 puzzle
tabuleiro8x8Valores :: [[Int]]
tabuleiro8x8Valores = [[2,5,0,0,3,0,0,0],
                        [0,0,6,0,0,0,0,0],
                        [0,0,5,0,5,2,0,0],
                        [0,0,0,2,0,0,0,0],
                        [0,0,1,0,4,0,0,0],
                        [3,0,2,0,0,4,0,0],
                        [0,0,0,6,0,0,0,0],
                        [0,0,0,0,4,0,3,2]]
-- The groups of the 8x8 puzzle
tabuleiro8x8Grupos :: [[Int]]
tabuleiro8x8Grupos = [[0,1,1,1,1,2,3,3],
                        [0,0,5,1,2,2,4,4],
                        [6,7,5,8,2,2,9,9],
                        [6,10,5,5,5,2,9,9],
                        [6,10,5,11,11,11,9,12],
                        [13,10,14,11,14,14,12,12],
                        [13,13,14,14,14,15,15,12],
                        [13,13,16,16,15,15,15,12]]

-- -----------------------------------------------------------------------------------------------------

-- Second example, a 10x10 puzzle
tabuleiro10x10Valores :: [[Int]]
tabuleiro10x10Valores = [[5,0,2,0,2,0,3,1,3,1],
                        [0,4,0,1,0,5,0,5,0,4],
                        [7,5,1,7,0,0,3,1,3,0],
                        [0,4,0,0,0,0,0,0,0,3],
                        [2,0,3,4,0,2,0,0,4,0],
                        [5,0,2,0,6,0,0,0,0,0],
                        [0,1,3,0,1,0,0,4,0,3],
                        [6,7,0,3,0,1,4,0,0,1],
                        [4,0,3,0,4,0,0,0,0,3],
                        [0,1,0,2,0,6,2,0,2,1]]
-- The groups of the 10x10 puzzle
tabuleiro10x10Grupos :: [[Int]]
tabuleiro10x10Grupos = [[1,2,2,2,3,3,3,3,4,4],
                        [1,1,1,2,6,6,7,7,4,7],
                        [5,5,1,6,6,9,8,7,7,7],
                        [5,5,6,6,10,9,8,8,8,11],
                        [5,5,5,6,10,10,11,11,11,11],
                        [12,12,15,15,15,10,22,22,21,21],
                        [12,12,12,15,15,16,17,18,21,21],
                        [13,13,12,15,16,16,17,18,20,20],
                        [13,13,14,14,14,14,17,18,18,19],
                        [13,13,13,14,14,14,17,17,19,19]]