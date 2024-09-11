{-|
Module      : Tarefa2
Description : Valida jogo
Copyright   : Simão Azevedo Oliveira <a107322@alunos.uminho.pt>
              Gabriel Pinto Dantas <a107291@alunos.uminho.pt>

Módulo para a realização da Tarefa 2 de LI1 em 2023/24.
-}
module Tarefa2 where
import LI12324
    ( Bloco(Vazio, Plataforma,Escada,Alcapao),
      Entidade(Fantasma, Jogador),
      Jogo(jogador, mapa, inimigos),
      Mapa(..),
      Personagem(tamanho, ressalta, tipo, vida, posicao) )
import Tarefa1

valida :: Jogo -> Bool
valida j = validaMapa (mapa j) 
            && validaSpawn j 
            && validaPersonagens (jogador j)
            && not (any (==False) (map (validaPersonagens) (inimigos j)))
            && validaInimigos j 
            && vidaFantasmas (inimigos j)
            && validaAlcapao j
            && not (any (==False) (map (vidaFantasma) (inimigos j)))
            && validaSpawnPersonagens (jogador j) (mapa j) 


--1
validaMapa :: Mapa -> Bool
validaMapa (Mapa ((xi,xf),d) (yi,yf) b) = filter (==Plataforma) (last b) == last b

--2
validaPersonagens :: Personagem -> Bool
validaPersonagens personagem | (tipo personagem == Fantasma && ressalta personagem == True) = True
                             | (tipo personagem == Jogador && ressalta personagem == False) = True
                             | otherwise = True

--3
validaSpawn :: Jogo -> Bool
validaSpawn j = not (any (==True) (map (colisoesPersonagens (jogador j)) (inimigos j)))

spawnPrincesa :: Mapa -> (Double,Double)
spawnPrincesa (Mapa ((xi,yi), d) (xf,yf) b) = (xf,yf)



--4
validaInimigos :: Jogo -> Bool
validaInimigos j = length (inimigos j) >= 2

--5
vidaFantasma :: Personagem -> Bool
vidaFantasma  p1 = case tipo p1 of
    Fantasma -> vida p1 == 1
    _ ->  True

vidaFantasmas :: [Personagem] -> Bool
vidaFantasmas [] = True
vidaFantasmas (h:t) | vidaFantasma h = vidaFantasmas t
                    | otherwise = False

--7
validaAlcapao :: Jogo -> Bool
validaAlcapao j = (fst (tamanho (jogador j))) <= 1


--8
validaSpawnPersonagens :: Personagem -> Mapa -> Bool
validaSpawnPersonagens p (Mapa ((xi,xf),d) (yi,yf) b) = verificaBloco p b == Vazio




{- Esta função retorna o bloco no qual o personagem está posicionado -}

verificaBloco :: Personagem -> [[Bloco]] -> Bloco
verificaBloco p b = getBloco b (negate (ceiling y)) (floor x) 
    where (x,y) = posicao p 


getBloco :: [[Bloco]] -> Int -> Int -> Bloco
getBloco lb linha coluna = lb !! linha !! coluna


