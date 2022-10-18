{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
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
    gety :: Int
}

type Tabuleiro = [[Numero]] -- lista de listas de Numero

makeTabuleiro :: Tabuleiro  -- por enquanto é valido somente para 8x8
makeTabuleiro = [[Numero (readVal i j) (readGrupos i j) i j | j <- [0..7]] | i <- [0..7]]

readGrupos :: Int -> Int -> Int -- por enquanto é valido somente para 8x8
readGrupos i j = (tabuleiro8x8Grupos !! i) !! j

readVal :: Int -> Int -> Int    -- por enquanto é valido somente para 8x8
readVal i j = (tabuleiro8x8Valores !! i) !! j

linhaToString :: [Numero] -> String
linhaToString [] = "\n"
linhaToString (atual:resto) = show(getValor atual) ++ linhaToString resto

tabToString :: Tabuleiro -> String
tabToString [] = ""
tabToString (linha:resto) = (linhaToString linha) ++ (tabToString resto)

printTabuleiro :: Tabuleiro -> IO()
printTabuleiro tab = putStrLn (tabToString tab)

main :: IO ()
main = do
    printTabuleiro makeTabuleiro