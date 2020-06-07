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