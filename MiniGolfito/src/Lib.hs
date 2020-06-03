module Lib where
import Text.Show.Functions

laVerdad = True


------------------------------------------------------------------------------------------------------------------------------
{-
filtrarPalabrasTerminadasEn :: String -> [String] -> [String]
filtrarPalabrasTerminadasEn terminacion = filter (not.(palabraTerminadaEn terminacion))

palabraTerminadaEn :: String -> String -> Bool
palabraTerminadaEn terminacion palabra =  ( drop (ultimosNDigitos terminacion palabra) palabra ) == terminacion

ultimosNDigitos :: String -> String -> Int
ultimosNDigitos terminacion palabra = (length palabra - length terminacion) -}

------------------------------------------------------------------------------------------------------------------------------

---- MODELO DE PARCIAL: MINIGOLFITO 

-- Modelo inicial

data Jugador = UnJugador {
  nombre :: String,
  padre :: String,
  habilidad :: Habilidad
} deriving (Eq, Show)

data Habilidad = Habilidad {
  fuerzaJugador :: Int,
  precisionJugador :: Int
} deriving (Eq, Show)

-- Jugadores de ejemplo
bart = UnJugador { nombre ="Bart", padre ="Homero", habilidad = Habilidad 25 60}
todd = UnJugador {nombre="Todd", padre = "Ned", habilidad = (Habilidad 15 80)}
rafa = UnJugador { nombre ="Rafa", padre= "Gorgory", habilidad = (Habilidad 10 1)}

data Tiro = UnTiro {
  velocidad :: Int,
  precision :: Int,
  altura :: Int
} deriving (Eq, Show)

type Puntos = Int
type Palo = Habilidad -> Tiro

configurarTiro :: Int -> Int -> Int -> Tiro
configurarTiro velocidad precision altura = UnTiro {velocidad = velocidad, precision = precision, altura = altura} 

putter :: Palo
putter habilidad = configurarTiro 10 (precisionJugador habilidad * 2) 0

madera :: Palo
madera habilidad = configurarTiro 100 (div (precisionJugador habilidad) 2) 5

hierro :: Int -> Palo
hierro n habilidad = configurarTiro (velocidadN (fuerzaJugador habilidad) n) (precisionN (precisionJugador habilidad) n) (alturaN n)

velocidadN :: Int -> Int -> Int
velocidadN fuerza n = fuerza * n
precisionN :: Int -> Int -> Int 
precisionN precision n = div precision n
alturaN :: Int -> Int 
alturaN n = max 0 (n-3) 

palos :: [Palo]
palos = [putter, madera, hierro 1, hierro 2, hierro 3 ,hierro 4, hierro 5, hierro 6, hierro 7, hierro 8, hierro 9, hierro 10]

golpe :: Palo -> Jugador -> Tiro
golpe palo = palo.habilidad

golpe':: Jugador -> Palo -> Tiro
golpe' jugador palo = palo (habilidad jugador)
-- PUNTO 3 -----

{-Un túnel con rampita sólo es superado si la precisión es mayor a 90 yendo al ras del suelo, independientemente de la velocidad del tiro. 
Al salir del túnel la velocidad del tiro se duplica, la precisión pasa a ser 100 y la altura 0. -}

type Obstaculo = Tiro -> Tiro

data TipoObstaculo = TunelConRampita | Laguna {largoLaguna ::Int}| Hoyo deriving (Show, Eq)

tunelConRampita :: Obstaculo
tunelConRampita (UnTiro velocidad precision altura) 
  | precision > 90 && altura == 0 = configurarTiro (velocidad * 2) 100 0
  | otherwise = tiroNulo

{-Una laguna es superada si la velocidad del tiro es mayor a 80 y tiene una altura de entre 1 y 5 metros. Luego de superar una laguna 
el tiro llega con la misma velocidad y precisión, pero una altura equivalente a la altura original dividida por el largo de la laguna.-}

laguna :: Int -> Obstaculo
laguna largoLaguna (UnTiro velocidad precision altura)
  | velocidad > 80 && between 1 5 altura = configurarTiro velocidad precision (div altura largoLaguna) 
  | otherwise = tiroNulo

between n m x = elem x [n .. m]
{-Un hoyo se supera si la velocidad del tiro está entre 5 y 20 m/s yendo al ras del suelo con una precisión mayor a 95.
 Al superar el hoyo, el tiro se detiene, quedando con todos sus componentes en 0.-}

hoyo :: Obstaculo
hoyo (UnTiro velocidad precision altura)
  | between 5 20 altura && precision > 95 && altura == 0 = tiroNulo
  | otherwise = configurarTiro velocidad precision altura 


comoQueda :: TipoObstaculo -> Tiro -> Tiro
comoQueda TunelConRampita tiro = tunelConRampita tiro
comoQueda (Laguna largoLaguna) tiro = laguna largoLaguna tiro
comoQueda Hoyo tiro = hoyo tiro

puedeSuperar :: TipoObstaculo -> Tiro -> Bool
puedeSuperar TunelConRampita tiro = not (tunelConRampita tiro == tiroNulo)
puedeSuperar (Laguna largoLaguna) tiro = not (laguna largoLaguna tiro == tiroNulo) 
puedeSuperar Hoyo tiro = not (hoyo tiro == tiro) 

tiroNulo = configurarTiro 0 0 0
tiroPerfecto = configurarTiro 100 100 0
tiroPerfecto' = configurarTiro 100 100 3

--  PUNTO 4 -----
-- Definir palosUtiles que dada una persona y un obstáculo, permita determinar qué palos le sirven para superarlo.

palosUtiles :: TipoObstaculo -> Jugador -> [Palo] 
palosUtiles TunelConRampita jugador = filter ((puedeSuperar TunelConRampita).(golpe' jugador)) palos
palosUtiles (Laguna largoLaguna) jugador = filter ((puedeSuperar (Laguna largoLaguna)).(golpe' jugador)) palos
palosUtiles Hoyo jugador = filter ((puedeSuperar Hoyo).(golpe' jugador)) palos

-- Saber, a partir de un conjunto de obstáculos y un tiro, cuántos obstáculos consecutivos se pueden superar.
-- BONUS: resolver este problema sin recursividad,
-- teniendo en cuenta que existe una función takeWhile :: (a -> Bool) -> [a] -> [a] que podría ser de utilidad.

cuantosSuperaDe :: [TipoObstaculo] -> Tiro -> Int
cuantosSuperaDe listaObstaculos tiro = length (listaDeSuperados listaObstaculos tiro )

listaDeSuperados :: [TipoObstaculo] -> Tiro -> [TipoObstaculo]
listaDeSuperados [] tiro = []
listaDeSuperados (x:xs) tiro | puedeSuperar x tiro = x : listaDeSuperados xs tiro | otherwise = listaDeSuperados xs (comoQueda x tiro)
                              | otherwise = []

tiroEjemplo = configurarTiro 10 95 0

-- Definir paloMasUtil que recibe una persona y una lista de obstáculos y determina cuál es el palo que le permite superar más obstáculos con un solo tiro.

paloMasUtil :: Jugador -> [TipoObstaculo] -> Palo
paloMasUtil jugador listaObstaculos = head (filter ( ( == (mayorNumeroDeSuperados jugador listaObstaculos palos)).(cuantosSuperaDe listaObstaculos).(golpe' jugador) ) palos )


mayorNumeroDeSuperados :: Jugador -> [TipoObstaculo] -> [Palo] -> Int
mayorNumeroDeSuperados jugador listaObstaculos palos =  maximum ( map (cuantosSuperaDe listaObstaculos)(map (golpe' jugador) palos ))

-- PUNTO 5 --

-- Dada una lista de tipo [(Jugador, Puntos)] que tiene la información de cuántos puntos ganó cada niño al finalizar el torneo, 
-- se pide retornar la lista de padres que pierden la apuesta por ser el “padre del niño que no ganó”.
-- Se dice que un niño ganó el torneo si tiene más puntos que los otros niños.

type Padre = String
type Resultado = (Jugador, Puntos)

listaPerdedores :: [Resultado] -> [Resultado]
listaPerdedores  lista = filter (\(x,y) -> not (y == (mayorPuntaje lista))) lista

padresPerdedores :: [Resultado] -> [Padre]
padresPerdedores lista = map (padre.fst) (listaPerdedores lista)

mayorPuntaje lista =  maximum (map snd lista)
