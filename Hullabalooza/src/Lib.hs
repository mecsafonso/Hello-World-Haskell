module Lib where
import Text.Show.Functions

laVerdad = True

{-

data Festival = UnFestival {
  lugar :: String,
  publico :: Publico,
  listaBandas :: [Banda]
} deriving (Show)


data Publico = UnPublico {
  cantidad :: Float,
  animoInicial :: String
} deriving ( Show)


data Genero =  RN | POP | HM | TM | SubgMet {terminacion :: String} deriving (Eq, Show)

type Descripcion = String

data Banda = UnaBanda {
  genero :: Genero,
  decibeles :: Int,
  descripcion :: [Descripcion]
} deriving (Show)

tocar :: Banda -> Publico -> Publico
tocar banda = alterarPublico (genero banda)

alterarPublico :: Genero -> Publico -> Publico
alterarPublico RN = agregarGente 100
alterarPublico POP = efectoPOP 
alterarPublico HM = agregarPorcentual 1.01 . efectoMetal "pesado"
alterarPublico TM = agregarPorcentual 1.01 . efectoMetal "basura"
alterarPublico (SubgMet terminacion) = efectoMetal terminacion

agregarGente :: Float -> Publico -> Publico
agregarGente agregado publ = publ {cantidad = cantidad publ + agregado}

efectoPOP :: Publico -> Publico 
efectoPOP publico | animoInicial publico == "indiferente" = modificarEstado "euforico". agregarGente (cantidad publico) $ publico
      | otherwise = publico

modificarEstado :: String -> Publico -> Publico
modificarEstado nuevoEstado publ = publ {animoInicial = nuevoEstado}  

efectoMetal :: String -> Publico -> Publico
efectoMetal terminacion publ = publ {animoInicial = animoInicial publ ++ " " ++ terminacion}

agregarPorcentual :: Float -> Publico -> Publico
agregarPorcentual porcentaje publ= publ {cantidad = (cantidad publ) * porcentaje}




--- PUNTO 2

-- Modelado de Ejemplos

  -- Los redondos, que está descripta como “legendaria” y “pogosa”, toca a 45 decibeles y se considera de rock nacional. 

redondos :: Banda
redondos = UnaBanda {descripcion = ["legendaria","grosa"], decibeles = 45, genero = RN}

  -- Soda está descripta como "irrepetible", toca a 40 decibeles y también es de rock nacional.

soda :: Banda
soda = UnaBanda {descripcion = ["irrepetible"], decibeles = 40 , genero = RN }

  -- Miranda es una banda de pop que toca a 60 decibeles y los críticos la describieron como "insípida", "incolora" e "inodora".
miranda :: Banda
miranda = UnaBanda {descripcion = ["insípida","incolora","inodora"], decibeles = 60, genero = POP }

  -- Metallica está descripta como “legendaria” y “vendida” y toca a 60 decibeles. Es una de las mayores exponentes del heavy metal.
metallica :: Banda
metallica = UnaBanda {descripcion = ["legendaria","vendida"], decibeles = 60, genero = HM }

  -- Agregar una banda que sea trash metal.
dillom :: Banda
dillom = UnaBanda {descripcion = ["Esto es trash", "Lo tuyo es basura"], decibeles = 100, genero = TM}

  -- Por ejemplo, el festival Hullabalooza, en el que tocan Miranda, Los Redondos, Metallica y Soda, 
  -- tiene un público de 20000 personas con ánimo inicial “indiferente”.

hullabalooza :: Festival 
hullabalooza = UnFestival {
  lugar = "Springfield", 
  publico = UnPublico {
    cantidad = 20000,
    animoInicial = "indiferente"
    },
  listaBandas = [miranda, redondos, metallica, soda]
} 
--- PUNTO 3

-- theStrokers = UnaBanda {descripcion = ["suicidio asistido", "emocional" , "linda"], decibeles = 45, genero = }










--- PUNTO 4

-- Definir la función suceder, que hace que suceda un festival. 
-- El resultado debe ser el mismo festival pero con el público en su situación final, luego de haber tocado todas las bandas. 

suceder :: Festival -> Festival
suceder festival = festival { publico = efectoFestival (publico festival) (listaBandas festival)}

efectoFestival :: Publico -> [Banda] -> Publico 
efectoFestival publ [] = publ
efectoFestival publ (x:xs) = efectoFestival (tocar x publ) xs


--- PUNTO 5 

  {- Se conocen ciertos criterios de clasificación de bandas, de los cuales depende su popularidad. Por ejemplo:

Vendida: Debe tener tres o más descripciones o bien una descripción que sea “vendida”. 
Acústica: Es la que toca a más de 55 decibeles. 
Legendaria. Debe estar descripta como “legendaria” y tocar a más de 40 decibeles.

Definir las funciones que permitan clasificar a las bandas. Una banda puede clasificarse de más de una manera a la vez o ninguna.
-}


type Clasificacion = String
type Criterio = ( (Banda -> Bool), Clasificacion)


clasificar :: Banda -> [Criterio] -> [Clasificacion] 
clasificar band [] = []
clasificar banda (x:xs) = laBandaEs x banda ++ clasificar banda xs  

laBandaEs :: Criterio -> Banda -> [Clasificacion]
laBandaEs criterio banda | fst criterio banda = [snd criterio]
  | otherwise = []

vendida :: Criterio
vendida = (condVendida, "Vendida")
acustica :: Criterio
acustica = (condAcustica, "Acustica")
legendaria :: Criterio
legendaria = (condLegendaria, "Legendaria")

condVendida :: Banda -> Bool 
condVendida band = elem "vendida" (descripcion band) || ( (>3).length.descripcion ) band
condAcustica :: Banda -> Bool
condAcustica = muchoRuido 55
condLegendaria :: Banda -> Bool
condLegendaria band = elem "legendaria" (descripcion band) && muchoRuido 40 band

muchoRuido :: Int -> Banda -> Bool
muchoRuido x = (> x) . decibeles 




--- PUNTO 6

  {- Definir la función popularidad, que, dada una lista de clasificaciones, permite conocer la popularidad de una banda. 
  La popularidad se calcula así: 100 puntos por cada clasificación a la que la banda aplique. -}

type Popularidad = Int

popularidad :: Banda -> [Criterio] -> Popularidad 
popularidad banda = (*100).(length).(clasificar banda)


--- PUNTO 7 

  {- Definir la función buenFest, que dado un festival y un conjunto de clasificaciones posibles dice si es un buen fest. 
  Esto sucede cuando cronológicamente cada banda es más popular que la anterior, y además 
  la popularidad total (la popularidad acumulada de sus bandas) supera los 1000 puntos.-}

buenFest :: Festival -> [Criterio] -> Bool
buenFest festival listaCriterios = cond1 festival listaCriterios && cond2 festival listaCriterios

cond1 :: Festival-> [Criterio] -> Bool
cond1 fest lista = popularidadCreciente lista . listaBandas $ fest 

popularidadCreciente :: [Criterio] -> [Banda] -> Bool
popularidadCreciente listaC  = ordenCreciente . map (flip popularidad listaC) 

ordenCreciente :: [Popularidad] -> Bool
ordenCreciente [] = True
ordenCreciente [x,y] = y > x 
ordenCreciente (x:y:xs) | y > x = ordenCreciente (y:xs) 
  | otherwise = False

cond2 :: Festival -> [Criterio] -> Bool
cond2 fest listaC=  (>1000).popularidadTotal listaC .listaBandas $ fest 

popularidadTotal :: [Criterio] -> [Banda] -> Int
popularidadTotal listaC = sum . map (flip popularidad listaC)

-}