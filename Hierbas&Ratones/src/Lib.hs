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