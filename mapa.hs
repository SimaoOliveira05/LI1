
module Mapa where

import Graphics.Gloss
    ( black,
      blue,
      magenta,
      red,
      white,
      yellow,
      color,
      pictures,
      scale,
      translate,
      display,
      Display(InWindow),
      Picture(Polygon) )
import Graphics.Gloss.Interface.Pure.Game
import Data.Maybe

data Direcao = Norte | Sul | Este | Oeste
type Posicao = (Double,Double)

data Bloco
    = Escada
    | Plataforma
    | Alcapao
    | Vazio
     deriving Eq
    
data Mapa = Mapa (Posicao, Direcao) Posicao [[Bloco]]

escada :: Picture
escada = color white (Polygon [(0,0),(1,0),(1,1),(0,1)])

plataforma :: Picture 
plataforma = color red (Polygon [(0,0),(1,0),(1,1),(0,1)])

alcapao :: Picture
alcapao = color blue (Polygon [(0,0),(1,0),(1,1),(0,1)])

desenhaPeca :: Bloco -> Picture 
desenhaPeca Escada = escada
desenhaPeca Plataforma = plataforma
desenhaPeca Alcapao = alcapao
desenhaPeca _ = plataforma

desenhaLinha :: [Bloco] -> Int -> [Picture]
desenhaLinha [] _ = []
desenhaLinha (h:t) x | h == Vazio = (desenhaLinha t (x+1))
                     | otherwise = (translate (fromIntegral(x*1)) 0 (desenhaPeca h)) : (desenhaLinha t (x+1))

desenhaMapa :: Mapa -> (Int,Int) -> [Picture]
desenhaMapa (Mapa ((xi,xf),d) (yi,yf) []) _ = []
desenhaMapa (Mapa ((xi,xf),d) (yi,yf) (h:t)) (x,y) = (translate 0 (fromIntegral(y*(-1))) (pictures (desenhaLinha h x))):(desenhaMapa (Mapa ((xi,xf),d) (yi,yf)  t ) (x,y+1))

mapaFixe :: Mapa
mapaFixe = (Mapa ((0,0), Oeste) (0,7) [[Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma]
           ,[Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio]
           ,[Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio]
           ,[Plataforma, Plataforma, Vazio, Vazio, Vazio, Vazio, Plataforma, Plataforma, Plataforma, Plataforma]
           ,[Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Escada, Vazio]
           ,[Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Escada, Vazio]
           ,[Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma]
           ])
       


mapaFixe' :: Picture 
mapaFixe' = scale 50 50 $ translate (-5) (3.5) ( pictures (desenhaMapa mapaFixe (0,0)))

displayMode :: Display
displayMode = InWindow "Game" (1000,500) (0,0)


main :: IO()
main = do display displayMode black desenhaJogo


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
    deriving Eq

type Velocidade = (Double,Double)

desenhaJogo :: Picture
desenhaJogo = pictures [player,mapaFixe',fantasma]

fantasma :: Picture 
fantasma = scale 50 50 $ color yellow (Polygon [(0,0),(0.5,0),(0.5,1),(0,1)])

player :: Picture
player = scale 50 50 $ translate (0) (-1.5) $ color magenta (Polygon [(0,0),(0.5,0),(0.5,1),(0,1)])


data Jogo = Jogo
    { mapa :: Mapa
    , inimigos :: [Personagem]
    , colecionaveis :: [(Colecionavel, Posicao)]
    , jogador :: Personagem
    }

data Colecionavel = Martelo | Estrela

--valida :: Jogo -> Bool
--valida jogo | 



validaMapa :: Mapa -> Bool
validaMapa (Mapa ((xi,xf),d) (yi,yf) b) | filter (==Plataforma) (last b) == last b = True
                                        | otherwise = False

validaPersonagens :: Personagem -> Bool
validaPersonagens personagem | (tipo personagem == Fantasma && ressalta personagem == True) = True
                             | (tipo personagem == Jogador && ressalta personagem == False) = True
                             | otherwise = True






desenhaPersonagem :: Personagem -> Picture
desenhaPersonagem p | tipo p == Fantasma = fantasma
                    | tipo p == Jogador = player

