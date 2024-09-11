
{-|
Module      : Tarefa1
Description : Verifica colisões
Copyright   : Simão Azevedo Oliveira <a107322@alunos.uminho.pt>
              Gabriel Pinto Dantas <a107291@alunos.uminho.pt>

Módulo para a realização da Tarefa 1 de LI1 em 2023/24.
-}
module Tarefa1 (colisoesParede, colisoesPersonagens,colisao) where

import LI12324


{-
A função 'colisoesParede' verifica se o personagem está a ultrapassar os limites do mapa, ou está em colisão com alguma plataforma

== Exemplos

>>> colisoesParede Mapa ((0,0) Oeste) (2,2) [[Vazio,Vazio,Vazio,Plataforma,Plataforma]] 
-}


colisoesParede :: Mapa -> Personagem -> Bool
colisoesParede (Mapa ((xi,xf),d) (yi,yf) b) personagem = (xp <= 0) || (xp + c >= fromIntegral (length (head b))) || (verificaPlataformas  personagem b)  || yp > 0
    where (xp,yp) = posicao personagem ;
          (c,a) = tamanho personagem

{- Esta função retorna True se o personagem ultrapassar a posição do bloco seguinte á sua posição , dependendo para onde ele está virado. 
 Utilizando esta função e a função "verificaPlataformas" conseguiremos impedir com que o personagem entre dentro de plataformas caso ambas as funções retornem True -}

colisaoBloco :: Personagem  -> Bool
colisaoBloco p
 | direcao p == Este && (x + c) > fromInteger (ceiling x) = True
 | direcao p == Oeste && x < fromInteger (floor (x+c)) = True
 | otherwise = False
  where (x,y) = posicao p ;
        (c,a) = tamanho p

-- Verifica se dois personagens colidiram entre si
colisoesPersonagens :: Personagem -> Personagem -> Bool
colisoesPersonagens p1 p2 = let (xp,yp) = posicao p1 ; (xf,yf) = posicao p2 ; (cp,ap) = tamanho p1; (cf,af) = tamanho p2;
                                in colisao ((xp,yp),(xp+cp,yp-ap)) ((xf,yf),(xf+cf,yf-af))

-- Verifica se duas hitboxes colidiram entre si , esta função é meramente auxiliar
colisao :: Hitbox -> Hitbox -> Bool
colisao ((x1,y1),(x2,y2)) ((x3,y3),(x4,y4)) = ((x3 >= x1 && x3 <= x2) || (x4 >= x1 && x4 <= x2) ) && ((y3 <= y1 && y3 >= y2) || (y4 >= y2 && y4 <= y1))

-- Esta função retorna True se o bloco  seguinte á posição do personagem é uma plataforma, dependendo de onde o personagem está virado

verificaPlataformas :: Personagem -> [[Bloco]] -> Bool
verificaPlataformas p b = case emEscada p of
      False ->  if direcao p == Este && getBloco b (negate (ceiling y)) (floor (x+c))  == Plataforma then True
                else if direcao p == Oeste && getBloco b (negate (ceiling y)) (floor x) == Plataforma then True
                else if direcao p == Norte && getBloco b (negate (ceiling (y+1))) (floor x) == Plataforma then True
                else if direcao p == Sul && getBloco b (negate (ceiling (y-1))) (floor x) == Plataforma then True
                else False
      True -> False
   
     where (x,y) = posicao p 
           (c,a) = tamanho p 

getBloco :: [[Bloco]] -> Int -> Int -> Bloco
getBloco lb linha coluna = lb !! linha !! coluna