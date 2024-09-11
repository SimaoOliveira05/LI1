{-# OPTIONS_GHC -Wno-missing-fields #-}
module Tarefa5 where

import LI12324
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Tarefa4 (atualiza)
import  Tarefa1 
import Tarefa3 (movimenta, verifica2BlocosAbaixo,verificaBloco',verifica2BlocosAbaixo',verificaBlocoAbaixo,verificaBlocoAbaixo',emVazio)
import System.Random
import System.Exit (exitFailure)
import Text.ParserCombinators.ReadP (get)
import Data.Char (GeneralCategory(InitialQuote))

data Menu = EmJogo | MenuInicial | GameOver | Victory 
data Opcoes = Jogar | Sair

data Tema = Claro | Escuro

data PrimateKong = PrimateKong { jogo :: Jogo
                                , menu :: Menu
                                , opcao :: Opcoes
                                , imagens :: [Picture]
                                , tema :: Tema
                                }

{-Aqui temos o Estado Inicial sendo composto por a nossa janela, mapa, os inimigos,colecionaveis, a princesa  e o macaco como tambem o jogo tudo respetivamente -}

window :: Display
window = FullScreen

mapaJogo :: Mapa
mapaJogo = Mapa ((17.5,-28),Oeste)(14,0)[[Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma]
                                        ,[Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma]
                                        ,[Plataforma,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Plataforma]
                                        ,[Plataforma,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Plataforma]
                                        ,[Plataforma,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Escada,Escada,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Plataforma]
                                        ,[Plataforma,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Escada,Escada,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Plataforma]
                                        ,[Plataforma,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Escada,Escada,Plataforma,Plataforma,Plataforma,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Plataforma]
                                        ,[Plataforma,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Escada,Escada,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Plataforma]
                                        ,[Plataforma,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Escada,Escada,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Plataforma]
                                        ,[Plataforma,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Escada,Escada,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Plataforma]
                                        ,[Plataforma,Vazio,Vazio,Vazio,Vazio,Vazio,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Vazio,Vazio,Vazio,Vazio,Vazio,Plataforma]
                                        ,[Plataforma,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Escada,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Escada,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Plataforma]
                                        ,[Plataforma,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Escada,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Escada,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Plataforma]
                                        ,[Plataforma,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Escada,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Escada,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Plataforma]
                                        ,[Plataforma,Vazio,Vazio,Vazio,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Alcapao,Plataforma,Plataforma,Alcapao,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Vazio,Vazio,Vazio,Plataforma]
                                        ,[Plataforma,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Escada,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Escada,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Plataforma]
                                        ,[Plataforma,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Escada,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Escada,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Plataforma]
                                        ,[Plataforma,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Escada,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Escada,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Plataforma]
                                        ,[Plataforma,Vazio,Vazio,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Alcapao,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Vazio,Vazio,Plataforma]
                                        ,[Plataforma,Vazio,Vazio,Vazio,Escada,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Escada,Vazio,Vazio,Vazio,Plataforma]
                                        ,[Plataforma,Vazio,Vazio,Vazio,Escada,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Escada,Vazio,Vazio,Vazio,Plataforma]
                                        ,[Plataforma,Vazio,Vazio,Vazio,Escada,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Escada,Vazio,Vazio,Vazio,Plataforma]
                                        ,[Plataforma,Vazio,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Alcapao,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Alcapao,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Vazio,Plataforma]
                                        ,[Plataforma,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Escada,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Escada,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Plataforma]
                                        ,[Plataforma,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Escada,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Escada,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Plataforma]
                                        ,[Plataforma,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Escada,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Escada,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Plataforma]
                                        ,[Plataforma,Vazio,Vazio,Vazio,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Alcapao,Plataforma,Plataforma,Plataforma,Plataforma,Alcapao,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Vazio,Vazio,Vazio,Plataforma]
                                        ,[Plataforma,Vazio,Vazio,Vazio,Vazio,Vazio,Escada,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Escada,Vazio,Vazio,Vazio,Vazio,Vazio,Plataforma]
                                        ,[Plataforma,Vazio,Vazio,Vazio,Vazio,Vazio,Escada,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Escada,Vazio,Vazio,Vazio,Vazio,Vazio,Plataforma]
                                        ,[Plataforma,Vazio,Vazio,Vazio,Vazio,Vazio,Escada,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Escada,Vazio,Vazio,Vazio,Vazio,Vazio,Plataforma]
                                        ,[Plataforma,Vazio,Vazio,Vazio,Vazio,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Vazio,Vazio,Vazio,Vazio,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Vazio,Vazio,Vazio,Vazio,Plataforma]
                                        ,[Plataforma,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Escada,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Escada,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Plataforma]
                                        ,[Plataforma,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Escada,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Escada,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Plataforma]
                                        ,[Plataforma,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Escada,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Escada,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Plataforma]
                                        ,[Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma]
                                        ,[Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma]]


enemies :: [Personagem]
enemies = [Personagem (-1.2,0) Fantasma (10,-9) Oeste (0.5,1) False False 1 0 (False,0) (False,0) 
          ,Personagem (1.2,0) Fantasma (10,-13) Oeste (0.5,1) False False 1 0 (False,0) (False,0)
          ,Personagem (-1.2,0) Fantasma (10,-13) Oeste (0.5,1) False False 1 0 (False,0) (False,0)
          ,Personagem (-1.2,0) Fantasma (28,-17) Oeste (0.5,1) False False 1 0 (False,0) (False,0)
          ,Personagem (1.2,0) Fantasma (10,-17) Oeste (0.5,1) False False 1 0 (False,0) (False,0)
          ,Personagem (1.2,0) Fantasma (27,-21) Oeste (0.5,1) False False 1 0 (False,0) (False,0)
          ,Personagem (1.2,0) Fantasma (4,-21) Oeste (0.5,1) False False 1 0 (False,0) (False,0)
          ,Personagem (-1.2,0) Fantasma (15,-21) Oeste (0.5,1) False False 1 0 (False,0) (False,0)
          ,Personagem (-1.2,0) Fantasma (10,-25) Oeste (0.5,1) False False 1 0 (False,0) (False,0)
          ,Personagem (1.2,0) Fantasma (24,-25) Oeste (0.5,1) False False 1 0 (False,0) (False,0)
          ,Personagem (-1.2,0) Fantasma (24,-29) Oeste (0.5,1) False False 1 0 (False,0) (False,0)
            ]

colecionaveisJogo :: [(Colecionavel,Posicao)]
colecionaveisJogo = [(Martelo,(17.5,-12.5)),(Moeda,(30,-12.8)),(Martelo,(5,-12.5)),(Martelo,(26,-4.5)),(Moeda,(13,-8.8)),(Moeda,(17.5,-24.8)),(Martelo,(4.5,-24.5)),(Martelo,(17.5,-20.5)),(Moeda,(6,-20.8)),(Moeda,(29,-20.8)),(Moeda,(14,-28.8)),(Martelo,(17.5,-32.5))]

posprincesa :: Posicao
posprincesa = (13,-4)

macacoMau :: Personagem
macacoMau = Personagem {
    velocidade = (0, 0),
    tipo = MacacoMalvado,
    posicao = (18, -3.8),
    tamanho = (3, 2),
    vida = 1,
    isImmune = (False,0)

     }

fr :: Int
fr = 60

           
jogoFixe :: Jogo
jogoFixe = Jogo{mapa =  mapaJogo, inimigos = enemies, colecionaveis = colecionaveisJogo, jogador = player , princesa = posprincesa, macaco = macacoMau}
    where
        player :: Personagem
        player = Personagem {
            velocidade = (0, 0),
            tipo = Jogador,
            posicao = (5, -33),
            direcao = Oeste,
            tamanho = (0.5, 1),
            emEscada = False,
            ressalta = False,
            vida = 3,
            pontos = 0,
            aplicaDano = (False, 0),
            isImmune = (False, 0)
            }

initialState :: [Picture] -> PrimateKong
initialState imgs = PrimateKong {
    jogo = jogoFixe,
    menu =  MenuInicial,
    opcao = Jogar,
    imagens = imgs,
    tema = Claro
}

{-A reageEventoPrimateKong é a função que vai conectar as acões no nosso teclado diretamente com o jogo tal nos menus como em jogo-}
reageEventoPrimateKong :: Event -> PrimateKong -> IO PrimateKong
-- movimentos dentro do jogo
reageEventoPrimateKong (EventKey (Char 'w') Down _ _) e@PrimateKong {menu = EmJogo}
    | emEscada (jogador (jogo e)) && (verificaBloco (jogador (jogo e)) (mapa (jogo e)) == Escada || verificaBloco' (jogador (jogo e)) (mapa (jogo e)) == Escada  )   = return $ e { jogo = atualiza [] (Just Subir) (jogo e) }
    | otherwise = return e
reageEventoPrimateKong (EventKey (Char 's') Down _ _) e@PrimateKong {menu = EmJogo}
    | emEscada (jogador (jogo e)) && (verifica2BlocosAbaixo (jogador (jogo e)) (mapa (jogo e)) == Escada || verifica2BlocosAbaixo' (jogador (jogo e)) (mapa (jogo e)) == Escada ) = return $ e { jogo = atualiza [] (Just Descer) (jogo e) }
    | otherwise = return e
reageEventoPrimateKong (EventKey (Char 'a') Down _ _) e@PrimateKong {menu = EmJogo} =
    return $ e { jogo = atualiza [] (Just AndarEsquerda) (jogo e) }
reageEventoPrimateKong (EventKey (Char 'd') Down _ _) e@PrimateKong {menu = EmJogo} =
    return $ e { jogo = atualiza [] (Just AndarDireita) (jogo e) }
reageEventoPrimateKong (EventKey (SpecialKey KeySpace) Down _ _) e@PrimateKong {menu = EmJogo} =
    return $ e { jogo = atualiza [] (Just Saltar) (jogo e) }
reageEventoPrimateKong (EventKey (SpecialKey KeyUp) Down _ _) e@PrimateKong {menu = EmJogo}
    | emEscada (jogador (jogo e)) && (verificaBloco (jogador (jogo e)) (mapa (jogo e)) == Escada || verificaBloco' (jogador (jogo e)) (mapa (jogo e)) == Escada  )   = return $ e { jogo = atualiza [] (Just Subir) (jogo e) }
    | otherwise = return e
reageEventoPrimateKong (EventKey (SpecialKey KeyDown) Down _ _) e@PrimateKong {menu = EmJogo}
    | emEscada (jogador (jogo e)) && (verifica2BlocosAbaixo (jogador (jogo e)) (mapa (jogo e)) == Escada || verifica2BlocosAbaixo' (jogador (jogo e)) (mapa (jogo e)) == Escada ) = return $ e { jogo = atualiza [] (Just Descer) (jogo e) }
    | otherwise = return e
reageEventoPrimateKong (EventKey (SpecialKey KeyLeft) Down _ _) e@PrimateKong {menu = EmJogo} =
    return $ e { jogo = atualiza [] (Just AndarEsquerda) (jogo e) }
reageEventoPrimateKong (EventKey (SpecialKey KeyRight) Down _ _) e@PrimateKong {menu = EmJogo} =
    return $ e { jogo = atualiza [] (Just AndarDireita) (jogo e) }
--- mexer nos menus
reageEventoPrimateKong (EventKey (SpecialKey KeyEsc) Down _ _) e@PrimateKong {menu = EmJogo} =
    return e {menu = MenuInicial}
reageEventoPrimateKong (EventKey (Char '1') Down _ _) e@PrimateKong {menu = MenuInicial} = 
    return e {menu = EmJogo}
reageEventoPrimateKong (EventKey (Char '2') Down _ _) e@PrimateKong {menu = MenuInicial} = exitFailure
reageEventoPrimateKong (EventKey (SpecialKey KeyEnter) Down _ _) e@PrimateKong {menu = GameOver} =
    return e {menu = MenuInicial}
reageEventoPrimateKong (EventKey (SpecialKey KeyEnter) Down _ _) e@PrimateKong {menu = Victory} =
    return e {menu = MenuInicial}
reageEventoPrimateKong _ pk = return $ pk { jogo = atualiza [] (Nothing) (jogo pk) } 

{- Aqui temos a reageTempo que irá tratar de outras funções que precisam constantemente de estar a ser chamadas como por exemplo verificar acontecimentos ou atualizar 
o estado do jogo consoante o tempo, a primeira condição existe para movimentar o jogo enquanto que a segunda serve para verificar tanto a derrota ou a vitoria do jogo 
atravês das funções auxiliares verificaFimdeJogo e verificaVitoria
 -}
tempo :: Float -> PrimateKong -> IO PrimateKong
tempo t e@PrimateKong { menu = EmJogo } = do
    s <- randomRIO (1, 100)
    let jogoAtualizado = movimenta s (realToFrac t) (jogo e)
    let primateKongAtualizado = e { jogo = jogoAtualizado }

    case (verificaFimDeJogo primateKongAtualizado, verificaVitoria primateKongAtualizado) of
        (GameOver, _) -> return primateKongAtualizado { menu = GameOver, jogo = jogoFixe }
        (_, Victory) -> return primateKongAtualizado { menu = Victory, jogo = jogoFixe }
        _ -> return primateKongAtualizado

tempo _ e = return e

verificaFimDeJogo :: PrimateKong -> Menu
verificaFimDeJogo e
    | vida (jogador (jogo e)) <= 0 = GameOver
    | otherwise = EmJogo

verificaVitoria :: PrimateKong -> Menu
verificaVitoria e
    | colisao ((xj,yj),(xj+c,yj-a)) ((xp,yp),(xp+1,yp-1.5)) = Victory 
    | otherwise = EmJogo
    where (xp,yp) = posprincesa
          (xj,yj) = posicao (jogador (jogo e))
          (c,a)   = tamanho (jogador (jogo e))

{-Esta é a função que irá se encarregar de desenhar devidamente o jogo em qualquer das suas fases sendo apoiada por as funções auxiliares definidas em baixo-}                                                           
desenhaPrimateKong :: PrimateKong -> IO Picture 
desenhaPrimateKong e@PrimateKong {menu = MenuInicial} = 
    return $ Pictures [opcaoJogar e, opcaoSair e, menuLogo e,fantasmaMenu e]
desenhaPrimateKong e@PrimateKong {menu = GameOver} =
    return $ Pictures [sadMario e,gameOver e,pressEnter e]
desenhaPrimateKong e@PrimateKong {menu = Victory} =
    return $ Pictures [victory e,trofeu e,marioBeijos e,pressEnter e]
desenhaPrimateKong e@PrimateKong { menu = EmJogo, imagens = imgs,jogo = Jogo { colecionaveis = colects}} = do
    let macacoPicture = desenhaMacaco e 
    let princesaPicture = desenhaPrincesa e
    let colectsPicture = pictures (desenhaColecionaveis e colects)
    let mapPicture = scale 1 1 (pictures (desenhaMapa e (mapa (jogo e)) (0, 0)))
    let playerPicture = desenhaPersonagem e
    let ghostsPicture = desenhaFantasmas e
    let livesPicture = scale 2 2 (desenhaVidas e)
    let pontosPicture = scale 0.25 0.25 (translate (2025) (-250) (color white (text ("Pontos:" ++ show (pontos (jogador (jogo e)))))))
    let countMartelo = if (fst (aplicaDano (jogador (jogo e)))) == True then scale 0.15 0.15 ((translate ((realToFrac x-0.56)*135) (((realToFrac y+1.7))*135)  (color white (text (show(round (snd (aplicaDano (jogador(jogo e)))))))))) else Blank where (x,y) = posicao (jogador (jogo e))
    return $ Translate (-940) 600 (scale 2.7 1.7 (pictures[mapPicture,playerPicture, ghostsPicture, livesPicture,colectsPicture,pontosPicture,princesaPicture,macacoPicture,countMartelo]))
--Translate (-1050) 650 (scale 3 2.2)
-- Translate (-940) 600 (scale 2.7 1.7 

{-Função utilizada para ir buscar imagens á lista encontrada no estado de Jogo (Primate Kong)-}
getImagem :: Int -> PrimateKong -> Picture
getImagem n PrimateKong { imagens = imgs } =
    if n < length imgs && n >= 0
        then imgs !! n
        else blank 

desenhaPeca :: PrimateKong -> Bloco -> Picture
desenhaPeca e Escada = getImagem 11 e
desenhaPeca e Plataforma = getImagem 12 e
desenhaPeca e Alcapao = getImagem 13 e
desenhaPeca e Vazio = getImagem 14 e

desenhaPersonagem :: PrimateKong -> Picture
desenhaPersonagem e@PrimateKong { jogo = Jogo { jogador = player } } 
    | emVazio player (mapa (jogo e))  = translate x y (scale 2 2 (getImagem 23 e)) 
    | fst (aplicaDano player) && dir == Oeste = translate x y (scale 2 2 (getImagem 9 e))
    | fst (aplicaDano player) && dir == Este = translate x y (scale 2 2 (getImagem 10 e))
    | verificaBloco (jogador (jogo e)) (mapa (jogo e)) == Escada || verificaBloco' (jogador (jogo e)) (mapa (jogo e)) == Escada || verificaBlocoAbaixo (jogador (jogo e)) (mapa (jogo e)) == Escada || verificaBlocoAbaixo' (jogador (jogo e)) (mapa (jogo e)) == Escada = translate x y (scale 2 2 (getImagem 8 e))
    | dir == Este && vx == 0 = translate x y (scale 2 2 (getImagem 5 e)) 
    | dir == Oeste && vx == 0 = translate x y (scale 2 2 (getImagem 4 e)) 
    | dir == Este && vx /= 0 = translate x y (scale 2 2 (getImagem 7 e)) 
    | dir == Oeste && vx /= 0 = translate x y (scale 2 2 (getImagem 6 e)) 
    | otherwise         = translate x y (scale 2 2 (getImagem 4 e)) 
 where
    (vx,vy) = velocidade player 
    dir = direcao player 
    (px, py) = posicao player
    (x,y) = (realToFrac px * 20,realToFrac py * 20 + 3)

desenhaVidas :: PrimateKong -> Picture
desenhaVidas e =
    case vida (jogador (jogo e)) of
        3 -> Translate 35 (-28) (scale 1.25 1.25 (getImagem 22 e))
        2 -> Translate 35 (-28) (scale 1.25 1.25 (getImagem 21 e))
        _ -> Translate 35 (-28) (scale 1.25 1.25 (getImagem 20 e))

{-A desenhaFantasmas funciona através de 2 funções, a pricipal que irá filtrar os fantasmas com vida superior a 0 da lista de inimigos e de seguida irá chamar a desenhaFantasma
individualmente para cada fantasma com a sua devida posição-}
desenhaFantasma :: Posicao -> PrimateKong -> Picture
desenhaFantasma (x, y) e =
    translate (realToFrac x * 20) (realToFrac y * 20) (scale 2.5 3 $ getImagem 15 e) 

desenhaFantasmas :: PrimateKong -> Picture
desenhaFantasmas e = pictures $ map (\fantasma -> desenhaFantasma (posicao fantasma) e) fantasmasComVida
  where
    fantasmasComVida = filter (\fantasma -> vida fantasma > 0) (inimigos (jogo e))

desenhaLinha :: PrimateKong -> [Bloco] -> Int -> [Picture]
desenhaLinha e [] _ = []
desenhaLinha e (h:t) x = translate (fromIntegral x*20) 0 (desenhaPeca e h) : (desenhaLinha e t (x+1))

desenhaMapa :: PrimateKong -> Mapa -> (Int,Int) -> [Picture]
desenhaMapa  e (Mapa ((xi,xf),d) (yi,yf) []) _ = []
desenhaMapa  e (Mapa ((xi,xf),d) (yi,yf) (h:t)) (x,y) =
  translate (fromIntegral x*16) (fromIntegral y*(-20)) (pictures (desenhaLinha e h x)) : desenhaMapa e (Mapa ((xi,xf),d) (yi,yf) t) (x, y+1)

desenhaColecionaveis :: PrimateKong -> [(Colecionavel, Posicao)] -> [Picture]
desenhaColecionaveis e [] = [] 
desenhaColecionaveis e ((Martelo, (x, y)):t) =
  translate (realToFrac x*20) (realToFrac y*20) (scale 2 2 (getImagem 16 e)) : desenhaColecionaveis e t
desenhaColecionaveis e ((Moeda, (x, y)):t) =
  translate (realToFrac x*20) (realToFrac y*20) (scale 1.5 1.5 (getImagem 17 e)) : desenhaColecionaveis e t

desenhaPrincesa :: PrimateKong -> Picture
desenhaPrincesa e@PrimateKong { jogo = Jogo { princesa = (x, y) } } =
    translate (realToFrac x * 20) (realToFrac y * 20 -12) $ scale 2 2 (getImagem 19 e)

{-A desenhaMacaco tambem é outra que funciona através de 2 funções, a principal irá verificar em primeiro lugar se o macaco  tem um y > -30, caso contrario o mesmo não sera desenhado,
se passar nessa condição será chamada a desenhaMacaco1 que irá verificar se o macaco está vivo ou morto para desenhar corretamente o mesmo.  -}
desenhaMacaco :: PrimateKong -> Picture 
desenhaMacaco e@PrimateKong { jogo = Jogo { macaco = monkey } } =
    if y > -30
        then desenhaMacaco1 e 
        else Blank 
    where (x,y) = posicao monkey 

desenhaMacaco1 :: PrimateKong -> Picture 
desenhaMacaco1 e@PrimateKong { jogo = Jogo { macaco = monkey } } 
   | vida monkey > 0 = translate (realToFrac x * 20) (realToFrac y * 20) (scale 2.5 2.5 (getImagem 24 e)) 
   | otherwise = translate (realToFrac x * 20) (realToFrac y * 20) (scale (0.57) (0.57) (getImagem 25 e))
   where (x,y) = posicao monkey 
 
gameOver :: PrimateKong -> Picture
gameOver e = Translate 10 450 (scale 2 2 (getImagem 29 e))

sadMario :: PrimateKong -> Picture 
sadMario e = Translate 0 (-50) (scale 2.5 2.5 (getImagem 28 e))

pressEnter :: PrimateKong -> Picture 
pressEnter e = Translate 0 (-450) (scale 1.25 1.25 (getImagem 30 e))

marioBeijos :: PrimateKong -> Picture 
marioBeijos e = Translate 0 (-150) (scale 1.25 1.25 (getImagem 18 e))

trofeu :: PrimateKong -> Picture 
trofeu e = Translate 25 450 (scale 1 1 (getImagem 27 e))

victory :: PrimateKong -> Picture 
victory e = Translate 10 150 (scale 1 1 (getImagem 26 e))

opcaoJogar :: PrimateKong -> Picture
opcaoJogar e = Translate 0 (50) (scale 1.25 1.25 (getImagem 1 e))

opcaoSair :: PrimateKong -> Picture
opcaoSair e = Translate 0 (-50) (scale 1.25 1.25 (getImagem 2 e))

menuLogo :: PrimateKong -> Picture
menuLogo e = translate 10 250 (scale 2 2 (getImagem 0 e))

fantasmaMenu :: PrimateKong -> Picture
fantasmaMenu e = pictures [translate (-400) 150 (scale 0.25 0.25 (getImagem 3 e)), translate 450 (-250) (scale 0.25 0.25 (getImagem 3 e))]

{-Função que carrega imagens para o Gloss-}
carregarImagens :: IO [Picture]
carregarImagens = do
  logoBMP <- loadBMP "logoMenu.bmp"
  jogarBMP <- loadBMP "jogarMenu.bmp"
  sairBMP <- loadBMP "sairMenu.bmp"
  fantasmaMenu <- loadBMP "fantasmaMenu.bmp"
  marioParadoEsquerda <- loadBMP "Walk0.bmp"
  marioParadoDireita <- loadBMP "Walk0direita.bmp"
  marioAndarEsquerda <- loadBMP "Walk1.bmp"
  marioAndarDireita <- loadBMP "Walk1direita.bmp"
  marioEscada <- loadBMP "Climbing2.bmp"
  marioMarteloEsquerda <- loadBMP "Hammer2.bmp"
  marioMarteloDireita <- loadBMP "Hammer2direita.bmp"
  escadaBMP <- loadBMP "Scale.bmp"
  plataformaBMP <- loadBMP "Floor.bmp"
  alcapaoBMP <- loadBMP "alcapao.bmp"
  vazioBMP <- loadBMP "vazio.bmp"
  fantasmaBMP <- loadBMP "fantasmaMau.bmp"
  marteloColect <- loadBMP "HammerColect.bmp"
  moedinha <- loadBMP "coin.bmp"
  marioVitoria <- loadBMP "mariobeijos2.bmp"
  paulineBMP <- loadBMP "pauline.bmp"
  vida1 <- loadBMP "1heart.bmp"
  vidas2 <- loadBMP "2hearts.bmp"
  vidas3 <- loadBMP "3hearts.bmp"
  marioSalta <- loadBMP "Saltar.bmp"
  macacoVivo <- loadBMP "aliveDK.bmp"
  macacoMorto <- loadBMP "deadDK.bmp"
  vitoria <- loadBMP "vitoria.bmp"
  trofeu <- loadBMP "trofeu.bmp"
  sadMario <- loadBMP "sadMario.bmp"
  gameOver <- loadBMP "gameOver.bmp"
  pressEnter <- loadBMP "pressenter.bmp"
  let imgs = [logoBMP,jogarBMP,sairBMP,fantasmaMenu,marioParadoEsquerda,marioParadoDireita,marioAndarEsquerda,marioAndarDireita,marioEscada,marioMarteloEsquerda,marioMarteloDireita,escadaBMP,
              plataformaBMP,alcapaoBMP,vazioBMP,fantasmaBMP,marteloColect,moedinha,marioVitoria,paulineBMP,vida1,vidas2,vidas3,marioSalta,macacoVivo,macacoMorto,vitoria,trofeu,sadMario,gameOver,
              pressEnter]
              --31 imagens
              
  return imgs


getBloco :: [[Bloco]] -> Int -> Int -> Bloco
getBloco lb linha coluna = lb !! linha !! coluna

verificaBloco :: Personagem -> Mapa -> Bloco
verificaBloco p (Mapa ((xi,xf),d) (yi,yf) b)  = getBloco b (negate (ceiling y)) (floor x) 
    where (x,y) = posicao p 