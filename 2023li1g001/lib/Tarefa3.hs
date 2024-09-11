{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
{-|
Module      : Tarefa3
Description : Movimenta personagens no jogo
Copyright   : Simão Azevedo Oliveira <a107322@alunos.uminho.pt>
              Gabriel Pinto Dantas <a107291@alunos.uminho.pt>

Módulo para a realização da Tarefa 3 de LI1 em 2023/24.
-}
module Tarefa3 where
import Tarefa1 ( colisoesParede, colisoesPersonagens, colisao )
import LI12324
    ( gravidade,Entidade(..),
      Bloco(..),
      Colecionavel(..),
      Direcao(Oeste, Este),
      Jogo(..),
      Mapa(..),
      Personagem(..),
      Posicao,
      Semente,Direcao(..),
      Tempo, Hitbox,Acao (AndarDireita , AndarEsquerda , Subir , Descer),geraAleatorios )
import Tarefa4 (atualiza)
import Data.List

{-Acho importante referir que adicionei os seguintes tipos:

No data Type Jogo adicionei o tipo princesa, que é apenas uma posição que representa a posição final do jogo (eu sei que os docentes 
colocaram essa posição no Data type mapa, mas achei bem mais simples apenas adicionar isso ao data type do jogo, e o macaco, que é um personagem
que, tal como a princesa, achei que ficaria mais simples apenas o adicionar ao jogo, sendo que irá existir apenas um, em vez de o colocar na lista
de inimigos, sendo que  o macaco irá ter propriedades especificas,  tal como cair por exemplo.

No data Type Personagem adicionei a componente "isImmune" que serve para dar um cooldown de imunidade ao personagem quando ele leva dano-}



movimenta :: Semente -> Tempo -> Jogo -> Jogo
movimenta s t jogo =
  let
    jogo1 = verificaEscadaInimigosMain jogo -- verifica se os inimigos estão numa escada
    jogo2 = verificaEscadaMain jogo1 -- verifica se o jogador está numa escada
    jogo3 = colisaoColecionaveisMain jogo2 -- tira os colecionaveis do mapa e aplica os respetivos efeitos ao jogador quando apanhados
    jogo4 = levarDanoMain jogo3 -- função que mexe com a vida do jogador quando colide com um fantasma
    jogo5 = veColisaoMain jogo4 -- fornece Pontos ao jogador quando elimina um fantasma
    jogo6 = darDanoMain jogo5 --
    jogo7 = ressaltaInimigoMain t jogo6 -- atualiza a componente ressalta aos inimigos (esta fica ativa caso o personagem não tenha mais plataforma por onde andar)
    jogo8 = mudaDirecaoMain jogo7 -- muda a direção de personagens caso a sua componente ressalta está ativa
    jogo9 = (if colisoesParede (mapa jogo8) (jogador jogo8 {jogador = (jogador jogo8) {posicao = (x+vx*t,y+vy*t)}}) -- movimenta o jogador
        then jogo8
        else jogo8 {jogador = (jogador jogo8) { posicao = (x+vx*t,y+vy*t)} })
     where
     (x,y) = posicao (jogador jogo8)
     (vx,vy) = velocidade (jogador jogo8)
    jogo10 = verificaComponentes t jogo9 -- verifica as componentes que contem tempo (Imunidade e Martelo)
    jogo11 = verificaEscadasMain jogo10 -- dá dano aos fantasmas caso em colisão com o martelo
    jogo12 = retiraAlcapaoMain jogo11  -- retira os alçapoes do mapa apenas quando o player passar por eles   
    jogo13 = movimentaInimigoMain t jogo12 -- atribui a lei das posiçoes aos inimigos
    jogo14 = caiMacacoMain jogo13 -- faz com que o macaco caia quando o jogador o mata
    jogo15 = darDanoMacacoMain jogo14 -- caso o jogador tenha martelo, permite matar o macaco
    jogo16 = levarDanoMacacoMain jogo15 -- caso o jogador tocar no macaco, o mesmo morre instantaneamente
    jogo17 = jogo16 {macaco = (macaco jogo16) {posicao = (x+vx*t,y+vy*t)}} -- atribui lei das posiçoes ao macaco  
     where (x,y) = posicao (macaco jogo16)
           (vx,vy) = velocidade (macaco jogo16)
    jogo18 = cairMain t jogo17 -- aplica gravidade aos personagens
         in geraMovimentosMain jogo18 -- faz com que os fantasmas pressigam o jogador
        

---------------------------------------------

--(eu entendi mal como funciona a componente ressalta dos personagens então não funciona de acordo com o que os professores pretendiam)

-- Muda a direção do inimigo caso não haja mais plataforma 

ressaltaInimigo' :: Tempo -> [Personagem] -> Mapa -> [Personagem]
ressaltaInimigo' _ [] _ = []
ressaltaInimigo' t (h:ti) m | tipo h == Fantasma = ressaltaInimigo t h m:ressaltaInimigo' t ti m
                            | otherwise = h:ressaltaInimigo' t ti m

ressaltaInimigo :: Tempo -> Personagem -> Mapa -> Personagem
ressaltaInimigo t i (Mapa ((xi,xf),d) (yi,yf) b)
  | colisoesParede (Mapa ((xi,xf),d) (yi,yf) b) (i {posicao = (x+vx*t,y+vy*t)}) = i {ressalta = True}
  | verificaBlocoAbaixoFrente i b == Vazio || verificaBlocoAbaixoTras i b == Vazio = i {ressalta = True }
  | verificaBloco i b /= Escada && emEscada i == True = i {posicao = (1,0),emEscada = False}
  | otherwise = i {ressalta = False}
  where
      (x, y) = posicao i
      (vx, vy) = velocidade i


mudaDirecao' :: [Personagem] -> [Personagem]
mudaDirecao' [] = []
mudaDirecao' (h:t) = mudaDirecao h:mudaDirecao' t

mudaDirecao :: Personagem -> Personagem
mudaDirecao f = case ressalta f of
    True -> f {velocidade = (-vx,vy),ressalta = False}
    False -> f
    where (vx,vy) = velocidade f


------------------------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------------------------

-- Faz com que o inimigo persiga o jogador, caso estejam na mesma plataforma

geraMovimentos :: [Personagem] -> Personagem -> [Personagem]
geraMovimentos [] _ = []
geraMovimentos (h:t) j | tipo h == Fantasma = geraMovimento h j:geraMovimentos t j
                       | otherwise = h:geraMovimentos t j

geraMovimento :: Personagem -> Personagem -> Personagem
geraMovimento fantasma jogador
  | colisao ((0,yj),(1,yj-aj)) ((0,yf),(1,yf-af)) && xf > xj = fantasma {velocidade = (-1,0)}
  | colisao ((0,yj),(1,yj-aj)) ((0,yf),(1,yf-af)) && (xf < xj) = fantasma {velocidade = (1,0)}
  | otherwise = fantasma
  where
      (xf, yf) = posicao fantasma
      (xj, yj) = posicao jogador
      (cf, af) = tamanho fantasma
      (cj, aj) = tamanho jogador


-------------------------------------------------

ressaltaInimigoMain :: Tempo -> Jogo -> Jogo
ressaltaInimigoMain t jogo = jogo {inimigos = ressaltaInimigo' t (inimigos jogo) (mapa jogo)}

mudaDirecaoMain :: Jogo ->  Jogo
mudaDirecaoMain jogo = jogo {inimigos = mudaDirecao' (inimigos jogo)}


geraMovimentosMain :: Jogo -> Jogo
geraMovimentosMain jogo = jogo {inimigos = geraMovimentos (inimigos jogo) (jogador jogo) }

retiraAlcapaoMain :: Jogo -> Jogo
retiraAlcapaoMain jogo = jogo {mapa = retiraAlcapao (jogador jogo)  (mapa jogo)}

verificaEscadaInimigosMain :: Jogo -> Jogo
verificaEscadaInimigosMain jogo = jogo {inimigos = verificaEscadaInimigos (inimigos jogo) (mapa jogo)}

verificaEscadaMain :: Jogo -> Jogo
verificaEscadaMain jogo = jogo {jogador = verificaEscadaJogador (jogador jogo) (mapa jogo)}

{- Aplica todas as funçoes que mexem com colecionaveis diretamente (meramente para simplificação -}
colisaoColecionaveisMain :: Jogo -> Jogo
colisaoColecionaveisMain jogo = jogo {jogador = efeitoColecionaveis (jogador jogo) (colecionaveis jogo) , colecionaveis = colisaoColecionaveis' (jogador jogo) (colecionaveis jogo) }

levarDanoMain :: Jogo -> Jogo
levarDanoMain jogo = jogo {jogador = levarDano (jogador jogo) (inimigos jogo)}


darDanoMain :: Jogo -> Jogo
darDanoMain jogo = jogo {inimigos = darDano (jogador jogo) (inimigos jogo)}

cairMain :: Tempo -> Jogo -> Jogo
cairMain t jogo = jogo {jogador = cair t (jogador jogo) (mapa jogo)}

verificaEscadasMain :: Jogo -> Jogo
verificaEscadasMain jogo = jogo {jogador = verificaEscadaJogador (jogador jogo) (mapa jogo),inimigos = verificaEscadaInimigos (inimigos jogo) (mapa jogo)}

 {-Função que calcula a nova posição de um inimigo -}

movimentaInimigo :: Tempo -> Personagem -> Personagem
movimentaInimigo t i =  i {posicao = (x+vx*t,y+vy*t)} where (x,y) = posicao i; (vx,vy) = velocidade i

movimentaInimigoMain :: Tempo -> Jogo -> Jogo
movimentaInimigoMain t jogo = jogo {inimigos = map (movimentaInimigo t) (inimigos jogo)}

caiMacacoMain :: Jogo -> Jogo
caiMacacoMain j = j {macaco = caiMacaco (macaco j)}

darDanoMacacoMain :: Jogo -> Jogo
darDanoMacacoMain j = j {macaco = darDanoMacaco (jogador j) (macaco j)}

levarDanoMacacoMain :: Jogo -> Jogo
levarDanoMacacoMain j = j {jogador = levarDanoMacaco (jogador j) (macaco j)}

veColisaoMain :: Jogo -> Jogo
veColisaoMain j = j {jogador = veColisao (jogador j) (inimigos j)}

verificaComponentes :: Tempo -> Jogo -> Jogo
verificaComponentes t jogo = let
    jogo1 = jogo { jogador = tempoMartelo t (jogador jogo)}
    jogo2 = jogo1 {jogador = tempoImunidade t (jogador jogo1)}
    jogo3 = jogo2 {jogador = verificaMartelo (jogador jogo2)}
     in jogo3 {jogador = verificaImunidade (jogador jogo3)}



------------------------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------------------------


{- Esta função é semelhante á função colisão definida na tarefa 1, só que em vez de testar a colisão entre o hitbox do personagem e o fantasma
verifica com a hitbox do martelo e o fantasma , isto se o jogador tiver um martelo ativo. 
Sendo a hitbox do martelo uma "caixa" diretamente á frente do jogador com exatamente o seu tamanho virada para o lado onde o personagem está. -}

colisaoMartelo :: Personagem -> Personagem -> Bool
colisaoMartelo jogador fantasma      | fst ( aplicaDano jogador) && direcao jogador == Este && fst (isImmune fantasma) == False  = let (xp,yp) = posicao jogador ; (xf,yf) = posicao fantasma ; (cp,ap) = tamanho jogador ; (cf,af) = tamanho fantasma
                                                                                                 in  colisao ((xp+cp,yp),(xp+2*cp,yp-ap)) ((xf,yf),(xf+cf,yf-af))
                                     | fst (aplicaDano jogador) && direcao jogador  == Oeste && fst (isImmune fantasma) == False = let (xp,yp) = posicao jogador ; (xf,yf) = posicao fantasma ; (cp,ap) = tamanho jogador ; (cf,af) = tamanho fantasma
                                                                                                 in  colisao ((xp-cp,yp),(xp,yp-ap)) ((xf,yf),(xf+cf,yf-af))
                                     |  otherwise = False


{-Função que determina a contagem do Martelo-}

tempoMartelo :: Tempo -> Personagem -> Personagem
tempoMartelo t p = if fst (aplicaDano p)
                    then p { aplicaDano = (True,tm - t)}
                        else p
    where (True,tm) = aplicaDano p

{-Retira o martelo do personagem, quando o tempo do mesmo chega a 0 -}

verificaMartelo :: Personagem -> Personagem
verificaMartelo p | tm <= 0 = p { aplicaDano = (False,0)}
                  | otherwise = p
    where tm = snd (aplicaDano p)

{- Funçao que , caso um fantasma da lista de inimigos do jogo colida com o martelo, vai retirar uma vida a esse fantasma. -}

darDano :: Personagem -> [Personagem] -> [Personagem]
darDano _ [] = []
darDano jogador (h:t)
    | vida h == 0 || fst (isImmune h) == True = h : darDano jogador t
    | colisaoMartelo jogador h = tiraVida h : darDano jogador t
    | otherwise = h : darDano jogador t


{-Fornece pontos ao jogador caso o mesmo mate um fantasma-}

veColisao :: Personagem -> [Personagem] -> Personagem
veColisao j [] = j
veColisao j (h:t) | colisaoMartelo j h = j {pontos = x+50}
                  | otherwise = veColisao j t
                where x = pontos j

{- Como era mais simples, resolvemos adicionar o record "macaco" ao jogo, em vez de o colocarmos na lista de inimigos

Estas funçoes fazem com que o personagem dê dano ao macaco e leve dano do mesmo tambem (tocar no macaco elimina instantaneamente as 3 vidas do jogar)-}

darDanoMacaco :: Personagem -> Personagem -> Personagem
darDanoMacaco jogador macaco | vida macaco == 0 = macaco
                             | colisaoMartelo jogador macaco = tiraVida macaco
                             | otherwise = macaco

levarDanoMacaco :: Personagem -> Personagem -> Personagem
levarDanoMacaco jogador macaco = levarDano' jogador macaco



{- Função auxiliar para a função levarDano -}
levarDano' :: Personagem -> Personagem -> Personagem
levarDano' jogador inimigo
    | vida inimigo > 0 && colisoesPersonagens jogador inimigo = if tipo inimigo == MacacoMalvado then tiraVidaMacaco jogador else tiraVida jogador
    | otherwise = jogador

{- Função que verifica se o jogador está em colisão com algum dos fantasmas da lista de inimigos do jogo. Retirando-lhe uma vida caso o mesmo aconteça. -}

levarDano :: Personagem -> [Personagem] -> Personagem
levarDano p [] = p
levarDano p (h:t) | colisoesPersonagens p h = levarDano' p h
                  | otherwise = levarDano p t


{- Função auxiliar que retira vida a um personagem e dá-lhe imunidade, neste caso, de 2 segundos-}
tiraVida :: Personagem -> Personagem
tiraVida personagem = case fst (isImmune personagem) of
    False -> personagem {vida = x - 1 , isImmune = (True,2)}
    True -> personagem
    where x = vida personagem

tiraVidaMacaco :: Personagem -> Personagem
tiraVidaMacaco personagem = case fst (isImmune personagem) of
    False -> personagem {vida = 0 }
    True -> personagem
    where x = vida personagem

{- Adicionei uma componente "isImmune" ao personagem, para existir um cooldown de imunidade quando o mesmo leva dano, esta função faz a contagem de esse cooldown -}
tempoImunidade :: Tempo -> Personagem -> Personagem
tempoImunidade t p = if fst (isImmune p)
                      then p {isImmune = (True, ti-t)}
                        else p
                    where ti = snd (isImmune p)

{- Retira a imunidade quando o cooldown chega a 0 -}
verificaImunidade :: Personagem -> Personagem
verificaImunidade p | ti <= 0 = p {isImmune = (False,0)}
                    | otherwise = p
                    where ti = snd (isImmune p)

------------------------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------------------------

{- Caso o personagem passe por cima de um alçapão, esta função faça com que o mesmo desapareça e passe a ser Vazio-}

retiraAlcapao :: Personagem -> Mapa -> Mapa
retiraAlcapao j (Mapa ((xi,xf),d) (yi,yf) b) = (Mapa ((xi,xf),d) (yi,yf) (retiraAlcapao' j (Mapa ((xi,xf),d) (yi,yf) b)))

retiraAlcapao' :: Personagem -> Mapa -> [[Bloco]]
retiraAlcapao' j (Mapa ((xi,xf),d) (yi,yf) b) | direcao j == Este && (verificaBlocoAbaixo j (Mapa ((xi,xf),d) (yi,yf) b) == Alcapao) = substituirElemento b (negate (ceiling y)+1) (floor x) Vazio
                    | direcao j == Oeste && getBloco b (negate (ceiling (y-a))) (floor (x+c)) == Alcapao = substituirElemento b (negate (ceiling y)+1) (floor (x+c)) Vazio
                    | otherwise = b
                where (x,y) = posicao j
                      (c,a) = tamanho j

-- Função principal para substituir um elemento na matriz
substituirElemento :: [[a]] -> Int -> Int -> a -> [[a]]
substituirElemento matriz linha coluna novoElemento
  -- Verifica se as coordenadas estão dentro dos limites da matriz
  | linha < 0 || coluna < 0 || linha >= length matriz || coluna >= length (head matriz) =
    error "Coordenadas inválidas."
  | otherwise =
    -- Atualiza a linha na posição especificada com o novo elemento
    take linha matriz ++
    [take coluna (matriz !! linha) ++ [novoElemento] ++ drop (coluna + 1) (matriz !! linha)] ++
    drop (linha + 1) matriz

------------------------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------------------------

{- Função que, caso o personagem esteja em cima de um bloco do tipo "Vazio" o mesmo cai-}

cair :: Tempo -> Personagem -> Mapa -> Personagem
cair t p m | emVazio p m && direcao p /= Norte = p {posicao = (x,y-4*gy*t)}
           | otherwise = p
    where (x, y) = posicao p ;
          (gx,gy) = gravidade

cairInimigos :: Tempo -> [Personagem] -> Mapa -> [Personagem]
cairInimigos tm (h:t) m | tipo h == Fantasma = cair tm h m:cairInimigos tm t m
                        | otherwise = h:cairInimigos tm t m

{-Função auxiliar que retorna True se o bloco abaixo do personagem é vazio-}

emVazio :: Personagem -> Mapa -> Bool
emVazio p (Mapa ((xi,xf),d) (yi,yf) b) | getBloco b (negate (ceiling (y-a))) (floor (x+c)) == Vazio && getBloco b (negate (ceiling (y-a))) (floor x) == Vazio = True
                                       | otherwise = False
                                                 where (x,y) = posicao p
                                                       (c,a) = tamanho p




------------------------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------------------------


{-Função que retorna True se o personagem está em colisão com um colecionavel, que neste caso ocupa um bloco inteiro -}

colisaoColecionaveis :: Personagem -> Posicao -> Bool
colisaoColecionaveis  p (xc,yc) = colisao ((x1,y1),(x2,y2)) ((xc,yc),(xc+1,yc-1))
    where (x1,y1) = posicao p;
          (x2,y2) = (x1+cp,y1-ap)
          (cp,ap) = tamanho p

{-Funçao que , caso o personagem colete um colecionavel, o mesmo é retirado da lista de colecionaveis do jogo-}

colisaoColecionaveis' :: Personagem -> [(Colecionavel, Posicao)] -> [(Colecionavel, Posicao)]
colisaoColecionaveis' p [] = []
colisaoColecionaveis' p ((c,(x,y)):t)       | colisaoColecionaveis p (x,y) = colisaoColecionaveis' p t
                                            | otherwise = (c,(x,y)):colisaoColecionaveis' p t

{-Função que aplica o efeito do colecionavel ao jogador, caso o mesmo o apanhe.-}

efeitoColecionaveis :: Personagem -> [(Colecionavel, Posicao)] -> Personagem
efeitoColecionaveis p [] = p
efeitoColecionaveis p ((Martelo,pc):xs) | colisaoColecionaveis p pc = p {aplicaDano = (True,t+10)}
                                        | otherwise = efeitoColecionaveis p xs
                       where t = snd (aplicaDano p)
efeitoColecionaveis p ((Moeda,pc):xs) | colisaoColecionaveis p pc = p {pontos = pt + 100}
                                      | otherwise = efeitoColecionaveis p xs
                        where pt = pontos p


------------------------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------------------------


{- Verifica se o inimigo está numa escada, e altera a sua componente "emEscada" para True -}

verificaEscadaInimigo :: Personagem -> Mapa -> Personagem
verificaEscadaInimigo p (Mapa ((xi,xf),d) (yi,yf) b) | (getBloco b (negate (ceiling y)) (floor x) == Escada || (getBloco b (negate (ceiling (y-a-1))) (floor x) == Escada || getBloco b (negate (ceiling (y-a-1))) (floor (x+1)) == Escada || getBloco b (negate (ceiling (y-a+1))) (floor (x+1)) == Escada || getBloco b (negate (ceiling (y-a+1))) (floor x) == Escada)) && (vy /= 0) = p {emEscada = True}
                                              | otherwise = p {emEscada = False}
                                                  where (x,y) = posicao p
                                                        (c,a) = tamanho p
                                                        (vx,vy) = velocidade p

verificaEscadaInimigos :: [Personagem] -> Mapa -> [Personagem]
verificaEscadaInimigos [] m = []
verificaEscadaInimigos (h:t) m = verificaEscadaInimigo h m : verificaEscadaInimigos t m




verificaEscadaJogador :: Personagem -> Mapa -> Personagem
verificaEscadaJogador p (Mapa ((xi,xf),d) (yi,yf) b) | (getBloco b (negate (ceiling y)) (floor x) == Escada || (getBloco b (negate (ceiling (y-a-1))) (floor x) == Escada || getBloco b (negate (ceiling (y-a-1))) (floor (x+1)) == Escada || getBloco b (negate (ceiling (y-a+1))) (floor (x+1)) == Escada || getBloco b (negate (ceiling (y-a+1))) (floor x) == Escada)) = p {emEscada = True}
                                              | otherwise = p {emEscada = False}
                                                  where (x,y) = posicao p
                                                        (c,a) = tamanho p
                                                        (vx,vy) = velocidade p

{-Função auxiliar que verifica 2 blocos diretamente abaixo do personagem (util para verificar se o personagem tem uma escada por baixo dele) -}
verifica2BlocosAbaixo :: Personagem -> Mapa -> Bloco
verifica2BlocosAbaixo p (Mapa ((xi,xf),d) (yi,yf) b) = getBloco b (negate (ceiling (y-a-1))) (floor x)
    where (x,y) = posicao p ;
          (c,a) = tamanho p

{-Igual é verifica2BlocosAbaixo, só que esta é para o outro lado da diagonal da hitbox do jogador -}

verifica2BlocosAbaixo' :: Personagem -> Mapa -> Bloco
verifica2BlocosAbaixo' p (Mapa ((xi,xf),d) (yi,yf) b) = getBloco b (negate (ceiling (y-a-1))) (floor (x+c))
    where (x,y) = posicao p ;
          (c,a) = tamanho p


{-Verifica o bloco que o personagem está diretamente posicionado em -}

verificaBloco :: Personagem -> [[Bloco]] -> Bloco
verificaBloco p b = getBloco b (negate (ceiling y)) (floor x)
    where (x,y) = posicao p
    
{-mesma coisa da verifica2BlocosAbaixo' -}

verificaBloco' :: Personagem -> Mapa -> Bloco
verificaBloco' p (Mapa ((xi,xf),d) (yi,yf) b) = getBloco b (negate (ceiling y)) (floor (x+c))
    where (x,y) = posicao p
          (c,a) = tamanho p


{-Função auxiliar que retorna o bloco diretamente abaixo do jogador -}
verificaBlocoAbaixo :: Personagem -> Mapa -> Bloco
verificaBlocoAbaixo p (Mapa ((xi,xf),d) (yi,yf) b) = getBloco b (negate (ceiling (y-a))) (floor x)
    where (x,y) = posicao p
          (c,a) = tamanho p

verificaBlocoAbaixo' :: Personagem -> Mapa -> Bloco
verificaBlocoAbaixo' p (Mapa ((xi,xf),d) (yi,yf) b) = getBloco b (negate (ceiling (y-a))) (floor (x+c))
    where (x,y) = posicao p
          (c,a) = tamanho p


verificaBlocoAbaixoFrente :: Personagem -> [[Bloco]] -> Bloco
verificaBlocoAbaixoFrente p b = getBloco b (negate (ceiling (y-a))) (floor (x+c))
    where (x,y) = posicao p
          (c,a) = tamanho p

verificaBlocoAbaixoTras :: Personagem -> [[Bloco]] -> Bloco
verificaBlocoAbaixoTras p b = getBloco b (negate (ceiling (y-a))) (floor (x))
    where (x,y) = posicao p
          (c,a) = tamanho p


{-A função mais importante deste código todo. :D -}

getBloco :: [[Bloco]] -> Int -> Int -> Bloco
getBloco lb linha coluna = lb !! linha !! coluna


------------------------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------------------------

{-Quando morto, o macaco cai -}

caiMacaco :: Personagem -> Personagem
caiMacaco p | vida p <= 0 = if ym > -34 then p {velocidade = (0,-10)} else p {velocidade = (0,0)}
            | otherwise = p
                where ym = snd (posicao p)


