module Lib where
import Text.Show.Functions

laVerdad = True

type Barrio = String
type Mail = String
type Requisito = Depto -> Bool
type Busqueda = [Requisito]

data Depto = Depto { 
  ambientes :: Int,
  superficie :: Int,
  precio :: Int,
  barrio :: Barrio
} deriving (Show, Eq)


ordenarSegun :: (a -> a -> Bool) -> [a] -> [a]

ordenarSegun _ [] = []
ordenarSegun criterio (x:xs) =
  (ordenarSegun criterio . filter (not . criterio x)) xs ++
  [x] ++
  (ordenarSegun criterio . filter (criterio x)) xs

between :: (Ord a) => a -> a -> a -> Bool
between cotaInferior cotaSuperior valor =
  valor <= cotaSuperior && valor >= cotaInferior

deptosDeEjemplo = [
  Depto 3 80 7500 "Palermo", 
  Depto 1 45 3500 "Villa Urquiza", 
  Depto 2 50 5000 "Palermo", 
  Depto 1 45 5500 "Recoleta"]


--- PUNTO 1
  --- 1a 
        {- Definir las funciones mayor y menor que reciban una función y dos valores, 
        y retorna true si el resultado de evaluar esa función sobre el primer valor es mayor o menor 
        que el resultado de evaluarlo sobre el segundo valor respectivamente.-}
        
mayor :: (Ord b) => ( a -> b ) -> a -> a -> Bool
mayor f a b = f a > f b
        
mayor' :: (Ord b) => ( a -> b ) -> a -> a -> Bool
mayor' f a = (f a > ).f 

menor :: (Ord b) => ( a -> b ) -> a -> a -> Bool
menor f a b = f a < f b

menor' :: (Ord b) => ( a -> b ) -> a -> a -> Bool
menor' f a = (f a <).f    
    
    --- 1b:
            {- Mostrar un ejemplo de cómo se usaría una de estas funciones para ordenar una lista de strings en base a su longitud usando ordenarSegun.-}

listaEjemplo1B :: [String]
listaEjemplo1B = ["hola","chau" , "quiero", "helado" , "."]

ejemplo1B :: [String]
ejemplo1B = ordenarSegun (mayor' length) listaEjemplo1B


--- PUNTO 2
-- Definir las siguientes funciones para que puedan ser usadas como requisitos de búsqueda:

    -- 2a
   -- ubicadoEn que dada una lista de barrios que le interesan al usuario, retorne verdadero 
   -- si el departamento se encuentra en alguno de los barrios de la lista.


ubicadoEn :: [Barrio] -> Requisito
ubicadoEn  listaBarrios = flip elem listaBarrios . barrio
ubibacoEn' listaBarrios depto = elem (barrio depto) listaBarrios -- Esta es la que nás me gusta
ubicadoEn'' listaBarrios depto = perteneceAAlguno depto $ listaBarrios
  where perteneceAAlguno depto = (any (== barrio depto)) 
  

    -- 2b
    -- cumpleRango que a partir de una función y dos números, indique si el valor retornado 
    --por la función al ser aplicada con el departamento se encuentra entre los dos valores indicados.

cumpleRango :: (Depto -> Int ) -> Int -> Int -> Requisito
cumpleRango f valorMin valorMax = (between valorMin valorMax).f


--- PUTNO 3

----- 3 A
        {- Definir la función cumpleBusqueda que se cumple si todos los requisitos de una búsqueda
         se verifican para un departamento dado. -}

cumpleBusqueda :: Busqueda -> Depto -> Bool
cumpleBusqueda busqueda = todoSeCumple . (aplicarRequisitos busqueda)

todoSeCumple :: [Bool] -> Bool
todoSeCumple = all ( == laVerdad)

aplicarRequisitos :: Busqueda -> Depto -> [Bool]
aplicarRequisitos [] depto = []
aplicarRequisitos (x:xs) depto = x depto : aplicarRequisitos xs depto

------ 3 A Alternativo : Esta resolución me parece mejor que l  a mía
--- cumpleBusqueda :: Depto -> Busqueda -> Bool
--- cumpleBusqueda depto busqueda = all (cumpleRequisito depto) busqueda
--- cumpleRequisito :: Depto -> Requisito -> Bool
--- cumpleRequisito depto requisito = requisito depto

----- 3 B
        {- Definir la función buscar que a partir de una búsqueda, un criterio de ordenamiento y una lista de departamentos 
        retorne todos aquellos que cumplen con la búsqueda ordenados en base al criterio recibido. -}

buscar :: Busqueda -> (Depto -> Depto -> Bool) -> [Depto] -> [Depto]
buscar busqueda criterio = (ordenarSegun criterio) . filter (cumpleBusqueda busqueda) 

----- 3 C
        {- Mostrar un ejemplo de uso de buscar para obtener los departamentos de ejemplo, ordenado por mayor superficie, que cumplan con:
            Encontrarse en Recoleta o Palermo 
            Ser de 1 o 2 ambientes 
            Alquilarse a menos de $6000 por mes -}

ejemplo3C :: [Depto]
ejemplo3C = buscar busquedaDelEjemplo3c (mayor superficie) deptosDeEjemplo

busquedaDelEjemplo3c :: Busqueda
busquedaDelEjemplo3c = [barrioCheto, dptoChiquito, baratuli]

barrioCheto :: Requisito
barrioCheto = ubicadoEn ["Palermo", "Recoleta"] 

barrioCheto' :: Requisito
barrioCheto' = barrioCheto'' . barrio 
barrioCheto'' :: Barrio -> Bool
barrioCheto'' x = x == "Palermo" || x == "Recoleta"

dptoChiquito :: Requisito
dptoChiquito = cumpleRango ambientes 1 2
dptoChiquito' :: Requisito
dptoChiquito' = (between 1 2 ). ambientes


baratuli :: Requisito
baratuli = cumpleRango precio 0 6000
baratuli' :: Requisito
baratuli' = (< 6000). precio
 
--- PUNTO 4

        {- Definir la función mailsDePersonasInteresadas que a partir de un departamento 
        y una lista de personas retorne los mails de las personas que tienen alguna búsqueda 
        que se cumpla para el departamento dado.-}


data Persona = Persona {
    mail :: Mail,
    busquedas :: [Busqueda]
}

mailsDePersonasInteresadas :: Depto -> [Persona] -> [Mail]
mailsDePersonasInteresadas depto [] = []
mailsDePersonasInteresadas depto (x:xs) | algunaBusquedaSeCumple depto (busquedas x) = mail x : mailsDePersonasInteresadas depto xs
   | otherwise = mailsDePersonasInteresadas depto xs

algunaBusquedaSeCumple :: Depto -> [Busqueda] -> Bool
algunaBusquedaSeCumple depto = any ( flip cumpleBusqueda depto) 

-- algunaBusquedaSeCumple depto = any ( cumpleBusqueda' depto) 
-- cumpleBusqueda' ::  Depto -> Busqueda -> Bool
-- cumpleBusqueda' depto busqueda = todoSeCumple . (aplicarRequisitos busqueda) $ depto


mailsDePersonasInteresadas' :: Depto -> [Persona] -> [Mail]
mailsDePersonasInteresadas' depto = map mail . filter (estaInteresada depto)

estaInteresada :: Depto -> Persona -> Bool
estaInteresada depto = any (flip cumpleBusqueda depto) . busquedas

--- Probando el punto 4

probandoPunto4 = mailsDePersonasInteresadas (Depto 1 45 5500 "Recoleta") [maxi,rasta,elo]
probandoPunto4' = mailsDePersonasInteresadas' (Depto 1 45 5500 "Recoleta") [maxi,rasta,elo]
rasta = Persona { mail = "elmaildelrastanolose", busquedas= [[baratuli, dptoChiquito]]} 
maxi = Persona { mail = "nofunciona" , busquedas = [[not.baratuli,not.dptoChiquito]]}
elo = Persona {mail = "elo", busquedas = [[not.baratuli],[barrioCheto]]}