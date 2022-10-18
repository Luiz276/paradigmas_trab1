{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use foldr" #-}
module Sudoku.Matrix where

import Sudoku.Input

data Casa = Casa {
    getValor :: Int,
    getX :: Int,
    getY :: Int
} deriving Eq

--type Tabuleiro = [[Numero]]

type Linha = [Casa]

type Grid = [Linha]


readInput :: Grid
readInput = [[Casa (readVal i j) i j | j <- [0..8]] | i <- [0..8]]  -- fixo para tabuleiros 9x9

readVal :: Int -> Int -> Int    -- por enquanto é valido somente para 9x9
readVal i j = (tabuleiro9x9Valores !! i) !! j

showCasa :: Grid -> Int -> Int-> Casa -- retorna elemento de uma casa do tabuleiro
showCasa grid i j = (grid !! i) !! j

linhaToString :: Linha -> String  -- converte linha para string
linhaToString [] = "\n"
linhaToString (atual:resto) = show(getValor atual) ++ linhaToString resto

gridToString :: Grid -> String  -- converte grid para string
gridToString [] = ""
gridToString (linha:resto) = (linhaToString linha) ++ (gridToString resto)

printTabuleiro :: Grid -> IO()  -- printa grid
printTabuleiro tab = putStrLn (gridToString tab)

main :: IO ()
main = do
  printTabuleiro readInput