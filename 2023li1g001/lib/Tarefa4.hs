{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
{-|
Module      : Tarefa4
Description : Atualiza as velocidades das personagens no jogo
Copyright   : Simão Azevedo Oliveira <a107322@alunos.uminho.pt>
              Gabriel Pinto Dantas <a107291@alunos.uminho.pt>

Módulo para a realização da Tarefa 4 de LI1 em 2023/24.
-}
module Tarefa4 where

import Data.Maybe
import Graphics.Gloss.Interface.IO.Interact

import LI12324
    ( Acao(Saltar, Subir, Descer, AndarEsquerda, AndarDireita),
      Direcao(Norte, Sul, Este, Oeste),
      Entidade(Fantasma),
      Jogo(..),
      Mapa(..),
      Personagem(..),
      Bloco(..) )
import Tarefa1 (colisoesParede)

atualiza :: [Maybe Acao] -> Maybe Acao -> Jogo -> Jogo
atualiza [] (Just Subir) jogo = jogo { jogador = (jogador jogo) { velocidade = (0,2)  } { direcao = Norte} }
atualiza [] (Just Descer) jogo = jogo { jogador = (jogador jogo) { velocidade = (0,-2) } { direcao = Sul } }
atualiza [] (Just AndarDireita) jogo = jogo { jogador = (jogador jogo) { velocidade = (2,y) } { direcao = Este}}
      where (x, y) = velocidade (jogador jogo)
atualiza [] (Just AndarEsquerda) jogo = jogo { jogador = (jogador jogo) { velocidade = (-2, y) } { direcao = Oeste }}
      where  (x, y) = velocidade (jogador jogo)
atualiza [] (Just Saltar) jogo | not (emVazio (jogador jogo) (mapa jogo)) = jogo { jogador = (jogador jogo) { velocidade = (vx,vy+7) } } -- O Jogador apenas pode saltar se estiver em cima de uma plataforma
                               | otherwise = jogo
                               where (vx,vy) = velocidade (jogador jogo)
atualiza [] Nothing jogo
  | fst (velocidade (jogador jogo)) > 0 = jogo { jogador = (jogador jogo) { velocidade = (x-2,y) } }
  | fst (velocidade (jogador jogo)) < 0 = jogo { jogador = (jogador jogo) { velocidade = (x+2,y) } }
  | otherwise = jogo { jogador = (jogador jogo) { velocidade = (0,0) } }
  where
      (x, y) = velocidade (jogador jogo)
atualiza ai Nothing jogo = jogo { inimigos = mexeInimigo ai (inimigos jogo) (mapa jogo)}




verificaBlocoAbaixo :: Personagem -> Mapa -> Bloco
verificaBlocoAbaixo p (Mapa ((xi,xf),d) (yi,yf) b) = getBloco b (negate (ceiling (y-a))) (floor x)
    where (x,y) = posicao p 
          (c,a) = tamanho p


{-Era para ser uma auxiliar para gerar movimento dos fantasmas aleatoriamente , mas não conseguimos-}

mexeInimigo :: [Maybe Acao] -> [Personagem] -> Mapa -> [Personagem]
mexeInimigo _ [] mapa = []
mexeInimigo (a1:ta) (h:t) mapa = mexeInimigoAux a1 h mapa :mexeInimigo ta t mapa

mexeInimigoAux ::  Maybe Acao -> Personagem -> Mapa -> Personagem
mexeInimigoAux m p mapa
 | ressalta p = p {velocidade = (-x,y)}
 | m == Just Subir && not (colisoesParede mapa p) = p {velocidade = (0,1),direcao = Norte}
 | m == Just Descer && not (colisoesParede mapa p)  = p {velocidade = (0,-1),direcao = Sul}
 | m == Just AndarDireita && not (colisoesParede mapa p) = p {velocidade = (1,0),direcao = Este}
 | m == Just AndarEsquerda && not (colisoesParede mapa p) =  p {velocidade = (-1,0),direcao = Oeste}
 | otherwise = p
                    where (x,y) = posicao p


--------------------------------------------------------

getBloco :: [[Bloco]] -> Int -> Int -> Bloco
getBloco lb linha coluna = lb !! linha !! coluna

emVazio :: Personagem -> Mapa -> Bool
emVazio p (Mapa ((xi,xf),d) (yi,yf) b) | getBloco b (negate (ceiling (y-a))) (floor (x+c)) == Vazio && getBloco b (negate (ceiling (y-a))) (floor x) == Vazio = True
                                       | otherwise = False
                                                 where (x,y) = posicao p 
                                                       (c,a) = tamanho p
