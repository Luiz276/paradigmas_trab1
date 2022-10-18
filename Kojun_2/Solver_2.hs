{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Kojun_2.Solver_2 where

import Kojun_2.Matrix

-- funções para resolver o kojun

lerGrupo :: Int -> Int -> [[Int]] -> Int
lerGrupo x y grupos = (grupos !! x) !! y

numeroVazio :: Numero -> Bool   -- testa se o numero é vazio
numeroVazio num = (getValor num)==0

tamGrupo :: Tabuleiro -> Numero -> Int  -- retorna tamanho do grupo
tamGrupo tab num = sum [1 | row<-tab, element<-row, ((getGrupo element) == (getGrupo num))]

inRow :: [Numero] -> Numero -> Bool -- checa se o Numero existe na linha
inRow [] _ = False
inRow (a:b) num | (getValor a == getValor num) && (getGrupo a == getGrupo num) && (getX a == getX num) && (getY a == getY num) = True
                | otherwise = inRow b num

inGrupo :: Tabuleiro -> Numero -> Bool  -- checa se o Numero existe no grupo
inGrupo [] _ = False
inGrupo (line:resto) num | inRow line num = True
                         | otherwise = inGrupo resto num

cimaBaixo :: Tabuleiro -> Numero -> Bool
cimaBaixo tab num | (getX num /= 0) && (getGrupo num == getGrupo (getCasa tab ((getX num)+1) (getY num))) && (getValor num < getValor (getCasa tab ((getX num)+1) (getY num))) = True
                  | otherwise = False

main :: IO ()
main = do
    print(tamGrupo makeTabuleiro (getCasa makeTabuleiro 0 1))