module Lib where
import Text.Show.Functions

laVerdad = True


--- PRIMERA PARTE
  --- PUNTO 1
  -- han pedido modelar los guanteletes que ellos producen en su herrería. Un guantelete está hecho de un material (“hierro”, “uru”, etc.) 
  -- y sabemos las gemas que posee. 
  
data Guantelete = UnGuantelete {
  material :: String, -- Tmb podria ser modelado con un data el material, pero como en el enunciado dice "etc" preferí no hacerlo
  gemas :: [Gema]
} deriving (Show)


  
  
  -- También se sabe de los personajes que tienen una edad, una energía,
  -- una serie de habilidades (como por ejemplo “usar espada”, “controlar la mente”, etc), su nombre y en qué planeta viven.

data Personaje = UnPersonaje {
  edad :: Int,
  energia :: Int,
  habilidades :: [Habilidad],
  nombre :: String,
  planeta :: Planeta
} deriving (Show)

type Planeta = String
type Habilidad = String


data Universo = UnUniverso {
  habitantes :: [Personaje]
} deriving (Show)

-- Los fabricantes determinaron que cuando un guantelete está completo -es decir, tiene las 6 gemas posibles- y su material es “uru”, 
-- se tiene la posibilidad de chasquear un universo que contiene a todos sus habitantes y reducir a la mitad la cantidad de dichos personajes.

chasquear :: Universo -> Universo
chasquear universo = universo { habitantes = reducirPoblación universo }
  where reducirPoblación universo =  take (div (cantidadHabitantes universo) 2) . habitantes $ universo

cantidadHabitantes :: Universo -> Int
cantidadHabitantes = length.habitantes


  --- PUNTO 2
      --Resolver utilizando únicamente orden superior.

-- Saber si un universo es apto para péndex, que ocurre si alguno de los personajes que lo integran tienen menos de 45 años.

aptoPendex :: Universo -> Bool
aptoPendex = not.null.filter esPendex.habitantes

esPendex :: Personaje -> Bool
esPendex = (<45).edad

-- Saber la energía total de un universo que es la sumatoria de todas las energías de sus integrantes que tienen más de una habilidad.
energiaTotal :: Universo -> Int
energiaTotal = sum . map energia . filter masDeUnaHabilidad . habitantes

masDeUnaHabilidad :: Personaje -> Bool
masDeUnaHabilidad = (>1).length.habilidades


--- Otra forma:
cumpleCondicion :: (Int -> Bool) -> (Personaje -> Int) -> Personaje -> Bool
cumpleCondicion f g x = f . g $ x

energiaTotal' :: Universo -> Int
energiaTotal' = sum . map energia . filter ( cumpleCondicion (>1) (length.habilidades) ) . habitantes

aptoPendex' :: Universo -> Bool
aptoPendex' = not . null . filter (cumpleCondicion (<45) edad) . habitantes



--- SEGUNDA PARTE  

  -- PUNTO 3 
  -- Implementar las gemas del infinito, evitando lógica duplicada. 

type Gema = Personaje -> Personaje

-- La mente que tiene la habilidad de debilitar la energía de un usuario en un valor dado.

mente :: Int -> Gema
mente valor = debilitarEnergia valor

debilitarEnergia ::  Int -> Personaje -> Personaje
debilitarEnergia valor pj = pj { energia = energia pj - valor }

-- El alma puede controlar el alma de nuestro oponente permitiéndole eliminar una habilidad en particular si es que la posee. 
-- Además le quita 10 puntos de energía.

alma :: Habilidad -> Gema
alma habilidadAEliminar = quitarHabilidad habilidadAEliminar 

quitarHabilidad :: Habilidad -> Personaje -> Personaje
quitarHabilidad habilidad pj = pj { habilidades = filter ( /= habilidad) (habilidades pj) }

-- El espacio que permite transportar al rival al planeta x (el que usted decida) y resta 20 puntos de energía.

espacio :: Planeta -> Gema
espacio planeta = teletransportar planeta

teletransportar :: Planeta -> Personaje -> Personaje
teletransportar nuevoPlaneta pj = pj { planeta = nuevoPlaneta}

-- El poder deja sin energía al rival y si tiene 2 habilidades o menos se las quita (en caso contrario no le saca ninguna habilidad).

poder :: Gema
poder = quitarNHabilidades 2 . desenergizar

desenergizar :: Personaje -> Personaje
desenergizar pj = pj { energia = 0}

quitarNHabilidades :: Int -> Personaje -> Personaje
quitarNHabilidades n pj | length (habilidades pj) <= n = pj {habilidades = []}
  | otherwise = pj

-- El tiempo que reduce a la mitad la edad de su oponente pero como no está permitido pelear con menores,
-- no puede dejar la edad del oponente con menos de 18 años. 
-- Considerar la mitad entera, por ej: si el oponente tiene 50 años, le quedarán 25. Si tiene 45, le quedarán 22 (por división entera). 
-- Si tiene 30 años, le deben quedar 18 en lugar de 15. También resta 50 puntos de energía.

tiempo :: Gema
tiempo = debilitarEnergia 50 . reducirEdad

reducirEdad :: Personaje -> Personaje
reducirEdad pj = pj {edad = max (edadReducida pj) 18}
  where edadReducida pj = edad pj - (div (edad pj)  2)

-- La gema loca que permite manipular el poder de una gema y la ejecuta 2 veces contra un rival.

gemaLoca :: Gema -> Gema
gemaLoca gema = gema . gema



--- PUNTO 4 
  -- Dar un ejemplo de un guantelete de goma con las gemas tiempo, 
   -- alma que quita la habilidad de “usar Mjolnir” y la gema loca que manipula el poder del alma tratando de eliminar la “programación en Haskell”.

guanteleteEjemplo = UnGuantelete {
  material = "goma",
  gemas = [tiempo, (alma "usar Mjolnir"), (gemaLoca (alma "programación en Haskell"))]
}

--- PUNTO 5
  -- No se puede utilizar recursividad. 
  -- Generar la función utilizar que dado una lista de gemas y un enemigo ejecuta el poder de cada una de las gemas que lo componen 
  -- contra el personaje dado. Indicar cómo se produce el “efecto de lado” sobre la víctima.

utilizar :: [Gema] -> Personaje -> Personaje
utilizar listaGemas enemigo = foldl (usarGema) enemigo listaGemas

usarGema :: Personaje -> Gema -> Personaje
usarGema x y = y x


--- PUNTO 6
  -- Resolver utilizando recursividad. 
  -- Definir la función gemaMasPoderosa que dado un guantelete y una persona 
  -- obtiene la gema del infinito que produce la pérdida más grande de energía sobre la víctima. 

gemaMasPoderosa :: Personaje -> Guantelete -> Gema
gemaMasPoderosa personaje = masAfecta personaje . gemas

masAfecta :: Personaje -> [Gema] -> Gema
masAfecta pj [] = undefined
masAfecta pj [x] = x
masAfecta pj (x:y:xs) | perdidaDeEnergia x pj > perdidaDeEnergia y pj = masAfecta pj (x:xs) 
  | otherwise = masAfecta pj (y:xs)

perdidaDeEnergia :: Gema -> Personaje -> Int
perdidaDeEnergia gem pj = energia pj - energia (usarGema pj gem) 


--- PUNTO 7
  -- No
  -- Si
      -- Lazy Evaluation


-- Probando, ejemplos:

drStrange = UnPersonaje {
  edad = 30,
  energia = 100,
  habilidades = ["programación en Haskell", "usar Mjolnir","no se"],
  nombre = "drStrange ... duh",
  planeta = "qsy"

}
