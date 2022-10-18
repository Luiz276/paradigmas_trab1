{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Kojun_2.Matrix where
-- Módulo responsável por organizar a matriz de entrada em um "tabuleiro" compatível com o arquivo solver

import Kojun_2.Input

-- cada elemento do tipo "Numero" representa uma casa do tabuleiro,
-- contendo o valor(numero) do local e a regiao a qual pertence,
-- além de coordenadas x e y para localizá-lo no tabuleiro
data Numero = Numero {
    getValor :: Int,
    getGrupo :: Int,
    getX :: Int,
    getY :: Int
} deriving Eq

type Tabuleiro = [[Numero]] -- lista de listas de Numero

-- Funções para conversão de lista de números para lista de data "Numero"

makeTabuleiro :: Tabuleiro  -- por enquanto é valido somente para 8x8
makeTabuleiro = [[Numero (readVal i j) (readGrupos i j) i j | j <- [0..7]] | i <- [0..7]]

readGrupos :: Int -> Int -> Int -- por enquanto é valido somente para 8x8
readGrupos i j = (tabuleiro8x8Grupos !! i) !! j

readVal :: Int -> Int -> Int    -- por enquanto é valido somente para 8x8
readVal i j = (tabuleiro8x8Valores !! i) !! j

-- Funções para output de matriz

linhaToString :: [Numero] -> String
linhaToString [] = "\n"
linhaToString (atual:resto) = show(getValor atual) ++ linhaToString resto

tabToString :: Tabuleiro -> String
tabToString [] = ""
tabToString (linha:resto) = (linhaToString linha) ++ (tabToString resto)

printTabuleiro :: Tabuleiro -> IO()
printTabuleiro tab = putStrLn (tabToString tab)

-- Funções para operações em Tabuleiro

getCasa :: Tabuleiro -> Int -> Int-> Numero -- retorna elemento de uma casa do tabuleiro
getCasa tab i j = (tab !! i) !! j

setCasa :: Tabuleiro -> Int -> Int -> Numero -> Tabuleiro   -- set elemento em uma casa do tabuleiro
setCasa tab i j num =
    take i tab ++ [take j (tab !! i) ++ [num] ++ drop (j+1) (tab !! i)] ++ drop (i+1) tab

comparaLinhas :: [Numero] -> [Numero] -> Bool   -- comparação entre linhas de um tabuleiro
comparaLinhas x y = (x == y)

comparaTabuleiro :: Tabuleiro -> Tabuleiro -> Bool  -- comparação entre tabuleiros
comparaTabuleiro [] [] = True
comparaTabuleiro (row1:mat1) (row2:mat2) | comparaLinhas row1 row2 = comparaTabuleiro mat1 mat2
                                         | otherwise = False

-- main :: IO ()
-- main = do
--     printTabuleiro makeTabuleiro