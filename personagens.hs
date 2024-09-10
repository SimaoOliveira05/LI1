module Personagens where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Data.Maybe
import Mapa

data Personagem = Personagem
    { velocidade :: Velocidade
    , tipo :: Entidade
    , posicao :: Posicao
    , direcao :: Direcao
    , tamanho :: (Double, Double)
    , emEscada :: Bool
    , ressalta :: Bool
    , vida :: Int
    , pontos :: Int
    , aplicaDano :: (Bool, Double)
    }

data Entidade = MacacoMalvado | Fantasma | Jogador

type Velocidade = (Double,Double)
type Posicao = (Float,Float)

data Direcao = N | S | E | O


fantasma :: Picture 
fantasma = color yellow (Polygon [(0,0),(40,0),(40,40),(0,40)])

jogador :: Picture
jogador = color magenta (Polygon [(0,0),(40,0),(40,40),(0,40)])




