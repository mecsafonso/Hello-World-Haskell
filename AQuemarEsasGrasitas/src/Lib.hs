module Lib where
import Text.Show.Functions()

data Gimnasta = UnGimnasta {
    nombre :: String, 
    edad :: Float,
    peso :: Float,
    tonif :: Float 
} deriving(Show)

pancho = UnGimnasta "Francisco" 40.0 120.0 1.0
andres = UnGimnasta "Andy" 22.0 80.0 6.0


{- Los ejercicios que se pueden hacer son funciones que dado un gimnasta y una cantidad de minutos, retorna a el gimnasta luego de realizar el ejercicio.
 Un ejemplo simple de ejercicio en el cual el gimnasta no hace nada (y por ende queda igual que al principio sin importar cuánto tiempo lo realice) 
 podría ser: -}

type Ejercicio = Float -> Gimnasta -> Gimnasta

relax :: Ejercicio
relax minutos gimnasta = gimnasta

-- PUNTO 1
-- Saber si alguien está saludable, lo cual se cumple si no está obeso y tiene una tonificación mayor a 5. Alguien es obeso si pesa más de 100 kilos. 

saludable :: Gimnasta -> Bool
saludable gimnasta = (not.estaObeso) gimnasta && bienTonificado gimnasta

estaObeso :: Gimnasta -> Bool
estaObeso = (>100).peso 

bienTonificado :: Gimnasta -> Bool
bienTonificado = (>5).tonif

-- PUNTO 2 
-- Hacer que el gimnasta queme una cantidad de calorías, lo que produce que baje de peso.
    -- Si el gimnasta es obeso, baja 1 kilo cada 150 calorías quemadas.
    -- Si no es obeso pero tiene más de 30 años y las calorías quemadas son más de 200, baja siempre un kilo.
    -- En cualquier otro caso se baja la cantidad de calorías quemadas dividido por el producto entre el peso y la edad del gimnasta. 

quemarCalorias :: Gimnasta -> Float -> Gimnasta 
quemarCalorias gimnasta calorias 
    | estaObeso gimnasta = perderPeso gimnasta (calorias / 150)
    | (not.estaObeso) gimnasta && esViejo gimnasta && calorias > 200 = perderPeso gimnasta 1
    | otherwise = perderPeso gimnasta (calorias / (peso gimnasta * edad gimnasta))

perderPeso :: Gimnasta -> Float -> Gimnasta
perderPeso gimnasta cantidadPerdida = gimnasta {peso = peso gimnasta - cantidadPerdida}

esViejo :: Gimnasta -> Bool
esViejo = (>30).edad

-- PUNTO 3
-- Desarrollar las funciones para los ejercicios caminataEnCinta, entrenamientoEnCinta, pesas, colina y montania sabiendo que:

-- La cinta quema calorías en función de la velocidad promedio alcanzada durante el ejercicio, quemando 1 caloría por la velocidad promedio por minuto.
    -- La caminata es un ejercicio en cinta con velocidad constante de 5 km/h. 

caminataEnCinta :: Ejercicio
caminataEnCinta duracion gimnasta = quemarCalorias gimnasta (1*5*duracion)

    -- El entrenamiento en cinta arranca en 6 km/h y cada 5 minutos incrementa la velocidad en 1 km/h, con lo cual la velocidad máxima dependerá de los 
    -- minutos de entrenamiento.

entrenamientoEnCinta :: Ejercicio
entrenamientoEnCinta duracion gimnasta = quemarCalorias gimnasta (caloriasQuemadasDelEntrenamiento duracion)
    where caloriasQuemadasDelEntrenamiento duracion = 1* ((6 + (velocidadMaximaAlcanzada duracion) ) / 2 ) * duracion

velocidadMaximaAlcanzada = (+6).(/5)

-- Las pesas tonifican la décima parte de los kilos a levantar si se realiza por más de 10 minutos, sino nada. 

pesas :: Float -> Ejercicio
pesas kilos duracion gimnasta | duracion > 10 = tonificar (tonif gimnasta + (kilos * 0.1)) gimnasta 
    | otherwise = gimnasta

tonificar :: Float -> Gimnasta  -> Gimnasta
tonificar cantidad  gimnasta  = gimnasta {tonif = tonif gimnasta + cantidad}
-- la colina quema 2 calorías por minuto multiplicado por la inclinación de la colina. 

colina :: Float -> Ejercicio
colina inclinacion duracion = flip quemarCalorias (2*duracion*inclinacion)


-- La montaña son 2 colinas sucesivas (cada una con la mitad de duración respecto de los minutos totales indicados), 
-- donde la segunda colina tiene una inclinación de 3 más que la inclinación inicial elegida. 
-- Además de hacer perder peso por las calorías quemadas por las colinas, este ejercicio incrementa en una unidad la tonificación de la gimnasta. 
    -- Resolver usando composición y aplicación parcial.

montaña :: Float -> Ejercicio
montaña inclinacionInicial duracion  = tonificar 1 . colina (inclinacionInicial + 3)  (duracion/2) . colina inclinacionInicial ( duracion/2 )  