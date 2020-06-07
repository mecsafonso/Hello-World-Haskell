module Lib where
import Text.Show.Functions

laVerdad = True

------------------------------------------------------------------------------------------------------------------------------

filtrarPalabrasTerminadasEn :: String -> [String] -> [String]
filtrarPalabrasTerminadasEn terminacion = filter (not.(palabraTerminadaEn terminacion))

palabraTerminadaEn :: String -> String -> Bool
palabraTerminadaEn terminacion palabra =  ( drop (ultimosNDigitos terminacion palabra) palabra ) == terminacion

ultimosNDigitos :: String -> String -> Int
ultimosNDigitos terminacion palabra = (length palabra - length terminacion) 

------------------------------------------------------------------------------------------------------------------------------

data Personaje = UnPersonaje {
  vida :: Int,
  arma :: Arma,
  armadura :: [Parte]
} deriving (Show)

type Parte = (Defensa, Durabildad)
type Defensa = Int
type Durabildad = Int


defensa :: Parte -> Int
defensa = fst
durabilidad :: Parte -> Int
durabilidad = snd


aumentarDefensaPorcentual :: Int -> Parte -> Parte
-- aumentarDefensaPorcentual porcentaje (defensa, durabilidad) = (defensa + porcentaje * defensa `div` 100, durabilidad)
aumentarDefensaPorcentual porcentaje = modificarDefensa (\defensa -> defensa + porcentaje * defensa `div` 100)

aumentarDefensa :: Int -> Parte -> Parte
-- aumentarDefensa cantidad (defensa, durabilidad) = (defensa + cantidad, durabilidad)
aumentarDefensa cantidad = modificarDefensa (+ cantidad)

-- Como también hay lógica repetida en las funciones anteriores, también
-- podemos abstraer:
modificarDefensa :: (Defensa -> Defensa) -> Parte -> Parte
modificarDefensa f (defensa, durabilidad) = (f defensa, durabilidad)  -- Con tuplas
-- modificarDefensa f parte = parte { defensa = f (defensa parte)}    -- Con data


restarDurabilidad :: Int -> Parte -> Parte
restarDurabilidad cantidad (defensa, durabilidad) = (defensa, (durabilidad - cantidad) `max` 0)


--- PUNTO 1

poderDefensa :: Personaje -> Int
poderDefensa personaje = vida personaje + defensaArmadura personaje

defensaArmadura :: Personaje -> Int
defensaArmadura = sum . map defensa . filter sirve . armadura

sirve :: Parte -> Bool
sirve = not . estaRota

estaRota :: Parte -> Bool
estaRota = (==0) . durabilidad


--- PUNTO 2

data Arma =
  Baculo { inteligencia :: Int, nombre :: String } |
  Arco   { rangoMaximo :: Int, longitudHilo :: Int, danioBase :: Int } |
  Espada { cantAlmas :: Int, material :: Material }
  deriving (Show)

data Material = Madera | Metal | Otro deriving (Eq, Show)


poderAtaque :: Personaje -> Int
poderAtaque = nivelAtaque . arma

nivelAtaque :: Arma -> Int
nivelAtaque (Baculo inteligencia nombre) = inteligencia + length nombre
nivelAtaque (Arco rango hilo danio) = rango * hilo + danio
nivelAtaque (Espada almas material) = almas * coeficienteForjado material

coeficienteForjado :: Material -> Int
coeficienteForjado Madera = 2
coeficienteForjado Metal = 3
coeficienteForjado _ = 1


--- PUNTO 3

type Buff = Personaje -> Personaje







frenesi :: Buff
frenesi = modificarArmadura (aumentarDefensaPorcentual 20)

modificarArmadura :: (Parte -> Parte) -> Personaje -> Personaje 
modificarArmadura f personaje = personaje { armadura = map f (armadura personaje) }



mantoEtereo :: Buff
mantoEtereo = restarVida 100 . modificarArmadura (aumentarDefensa 3)

restarVida :: Int -> Personaje -> Personaje
restarVida cantidad personaje = personaje { vida = vida personaje - cantidad }


berserker :: Buff
berserker = metalizarArma . modificarArmadura (modificarDefensa (\_ -> 2))

espejoKarma :: Buff -> Buff
espejoKarma buff = buff . buff

sucesionBuffs :: [Buff] -> Buff
sucesionBuffs buffs personaje = foldl potenciar personaje buffs

-- Otra forma de hacerlo (pensando en que construyo un Buff, y no un Personaje)
sucesionBuffsV2 buffs = foldl1 (.) buffs


potenciar :: Personaje -> Buff -> Personaje
potenciar personaje buff = buff personaje

esInofensivo :: [Personaje] -> Buff -> Bool
esInofensivo personajes buff = all (not . cambioSkills buff) personajes

-- Detectar si un buff es inofensivo, lo cual significa que al utilizarlo con un grupo de personajes de prueba, no altera el poder de ataque ni el de defensa de ninguno de ellos.
cambioSkills :: Buff -> Personaje -> Bool
cambioSkills buff personaje = 
  mismoAtaque personaje (potenciar personaje buff) && mismaDefensa personaje (potenciar personaje buff)

mismoAtaque = mismoSkill poderAtaque
mismaDefensa = mismoSkill poderDefensa

mismoSkill f personaje1 personaje2 = f personaje1 == f personaje2

metalizarArma :: Personaje -> Personaje
metalizarArma personaje = personaje { arma = metalizar (arma personaje) }

metalizar :: Arma -> Arma
metalizar (Espada almas Madera) = Espada almas Metal
metalizar arma = arma



--- PUNTO 4

desgastar :: Int -> Personaje -> Personaje
desgastar intensidad persona = persona { armadura = gastarArmadura intensidad (armadura persona) }

gastarArmadura :: Int -> [Parte] -> [Parte]
gastarArmadura _ [] = [] -- caso base
gastarArmadura intensidad (parte : partes) = restarDurabilidad intensidad parte : gastarArmadura (intensidad `div` 2) partes



--- PUNTO 5

data Clan = UnClan {
  miembros :: [Personaje],
  buffs :: [Buff]
}


leGana :: Clan -> Clan -> Bool
leGana clanAgresor clanDefensor = sumatoriaSegun poderAtaque clanAgresor > sumatoriaSegun poderDefensa clanDefensor

-- TODO: Abstraer lógica repetida
-- sumatoriaAtaque :: Clan -> Int
-- sumatoriaAtaque = sum . map poderAtaque . miembrosBuffadosSegun poderAtaque
-- sumatorioDefensa :: Clan -> Int
-- sumatorioDefensa = sum . map poderDefensa . miembrosBuffadosSegun poderDefensa

sumatoriaSegun f = sum . map f . miembrosBuffadosSegun f


-- miembrosBuffadosAtaque :: Clan -> [Personaje]
-- miembrosBuffadosAtaque = miembrosBuffadosSegun poderAtaque
-- miembrosBuffadosDefensa :: Clan -> [Personaje]
-- miembrosBuffadosDefensa = miembrosBuffadosSegun poderDefensa

miembrosBuffadosSegun :: (Personaje -> Int) -> Clan -> [Personaje]
miembrosBuffadosSegun criterio clan = map (buffearConMaximoCriterio criterio (buffs clan)) (miembros clan)


buffearConMaximoCriterio :: (Personaje -> Int) -> [Buff] -> Personaje -> Personaje
buffearConMaximoCriterio criterioDePersonaje buffs personaje = potenciar personaje buffMaximoAtaque
  where buffMaximoAtaque = foldl1 (maxDePotenciar criterioDePersonaje personaje) buffs

-- Estas funciones ya no tienen sentido, porque usar la función genérica me da la misma
-- declaratividad y expresividad.
-- maxAtaque :: Personaje -> Buff -> Buff -> Buff
-- maxAtaque = maxDePotenciar poderAtaque
-- maxDefensa :: Personaje -> Buff -> Buff -> Buff
-- maxDefensa = maxDePotenciar poderDefensa

maxDePotenciar criterioDePersonaje personaje = maxSegun (criterioDePersonaje . potenciar personaje)


maxSegun f buff1 buff2
  | f buff1 > f buff2 = buff1
  | otherwise = buff2