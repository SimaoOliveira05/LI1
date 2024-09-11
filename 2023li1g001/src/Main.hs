module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import LI12324
import Tarefa1
import Tarefa2 
import Tarefa3
import Tarefa4
import Tarefa5
 

main :: IO ()
main = do 
    imgs <- carregarImagens 
    let estadoInicial = initialState imgs
    playIO window black fr estadoInicial desenhaPrimateKong reageEventoPrimateKong tempo


