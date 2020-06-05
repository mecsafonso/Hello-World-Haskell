module Lib where
import Text.Show.Functions

laVerdad = True

type Enfermedad = String
type Nombre = String

data Raton = UnRaton {
    nombre :: Nombre,
    edad :: Float,
    peso :: Float,
    enfermedades :: [Enfermedad]
} deriving (Eq, Show)


-- PUNTO 1 
    --- Modelar a los ratones 
cerebro :: Raton
cerebro = UnRaton "Cerebro" 9 0.2 ["Brucelosis", "Sarampion","Tuberculosis"]
bicenterrata :: Raton
bicenterrata = UnRaton "Bicenterrata" 256 0.2 []
huesudo :: Raton
huesudo = UnRaton "Huesudo" 4 10 ["Obesidad","Sinusitis"]

-- PUNTO 2
    --- Existen distintos tipos de hierbas que afectan (modifican) de diferentes maneras al ratón. Definir dichas hierbas
type Hierba = Raton -> Raton

--- hierbaBuena, que rejuvenece al ratón a la raíz cuadrada de su edad.
--- Por ejemplo, si a cerebro le doy hierbaBuena, se transforma en un ratón de 3 años.

hierbaBuena :: Hierba
hierbaBuena raton = (rejuvenecerRaton raton) . sqrt . edad $ raton

rejuvenecerRaton :: Raton -> Float -> Raton
rejuvenecerRaton raton x = raton { edad = x} 

--- hierbaVerde, elimina las enfermedades que terminen de cierta forma.
---Por ejemplo, si a cerebro le doy la hierbaVerde del tipo “sis”, queda sólo con sarampión.

hierbaVerde :: String -> Hierba
hierbaVerde terminacion = quitarEnfermedadesTerminadasEn terminacion

quitarEnfermedadesTerminadasEn :: String -> Raton -> Raton 
quitarEnfermedadesTerminadasEn terminacion raton = raton { enfermedades = filtrarPalabrasTerminadasEn terminacion (enfermedades raton)}

filtrarPalabrasTerminadasEn :: String -> [String] -> [String]
filtrarPalabrasTerminadasEn terminacion = filter (not.(palabraTerminadaEn terminacion))

palabraTerminadaEn :: String -> String -> Bool
palabraTerminadaEn terminacion palabra =  ( drop (ultimosNDigitos terminacion palabra) palabra ) == terminacion

ultimosNDigitos :: String -> String -> Int
ultimosNDigitos terminacion palabra = (length palabra - length terminacion) 
 

---alcachofa, hace que el ratón pierda peso en un 10% si pesa más de 2kg, sino pierde un 5%.
---Por ejemplo, un raton de 3 kg queda con 2,7 kg y cerebro queda con 0.19 kg. 

alcachofa :: Hierba 
alcachofa raton | (>2).peso $ raton = quitarPesoPorcentual 10 raton
                | otherwise = quitarPesoPorcentual 5 raton

quitarPesoPorcentual :: Float -> Raton -> Raton 
quitarPesoPorcentual porcentaje raton = raton {peso = (peso raton) * (1 - (porcentaje * 0.01))}

---hierbaZort, hace que el ratón se transforme en Pinky, perdiendo todas sus enfermedades y quedando con 0 años de edad.

hierbaZort :: Hierba
hierbaZort raton = raton {
    nombre = "Pinky", 
    enfermedades = [], 
    edad = 0
}

---hierbaDelDiablo, hace que el ratón pierda 0.1kg (sin disminuir de 0) y elimina todas las enfermedades con menos de 10 letras. 

hierbaDelDiablo :: Hierba
hierbaDelDiablo = ( perderPeso 0.1 ).(quitarEnfermedadesCortas)

quitarEnfermedadesCortas :: Raton -> Raton
quitarEnfermedadesCortas raton = raton {
    enfermedades = filter ((>10).length) (enfermedades raton) 
}

perderPeso :: Float -> Raton -> Raton 
perderPeso cantidadAQuitar raton = raton { 
    peso = max 0 (peso raton - cantidadAQuitar)
}


--- PUNTO 3

-- Los medicamentos son la administración sucesiva de un conjunto de hierbas. 
-- Se pide crear los siguientes medicamentos para luego poder administrarlos en un ratón: 

-- Hacer el pondsAntiAge, que es un medicamento que está hecho con 3 hierbas buenas y una alcachofa. iterate
-- Por ejemplo, si se lo administramos al ratón Bicenterrata, queda con 2 años y 0.19 kg 

pondsAntiAge = hierbaBuena.hierbaBuena.hierbaBuena.alcachofa

-- Hacer el reduceFatFast, (que viene en distintas potencias) y es un medicamento compuesto por una hierbaVerde de “obesidad” 
-- y tantas alcachofas como indique su potencia.
-- Por ejemplo administrándole a Huesudo un reduceFatFast de potencia 1 hace que huesudo pase a pesar 9 kg y sólo quede con sinusitis. 
-- Si en lugar de la 1 le administramos un reduceFatFast de potencia 2, pasa a pesar 8.1 kg y queda también solo con sinusitis.

reduceFatFast :: Int -> Raton -> Raton
reduceFatFast potencia = (hierbaVerde "Obesidad").(aplicarNVeces potencia alcachofa)

aplicarNVeces :: Int -> Hierba -> Raton -> Raton
aplicarNVeces 0 hierba raton = raton
aplicarNVeces n hierba raton = aplicarNVeces (n-1) hierba (hierba raton)

-- Hacer la pdepCilina, que es un medicamento que usa hierbasVerdes para curar todas las enfermedades infecciosas. 
-- Las enfermedades infecciosas son aquellas cuyo nombre termina de alguna de estas formas (utilizar esta constante):
sufijosInfecciosas = [ "sis", "itis", "emia", "cocos"]

pdepCilina :: Raton -> Raton
pdepCilina raton =  foldl (aplicar) raton (map hierbaVerde sufijosInfecciosas)
    where aplicar x y = y x



