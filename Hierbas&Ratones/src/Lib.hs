module Lib where
import Text.Show.Functions

laVerdad = True

type Enfermedad = String
type Nombre = String

data Raton = UnRaton {
    nombre :: Nombre,
    edad :: Int,
    peso :: Float,
    enfermedades :: [Enfermedad]
} deriving (Eq, Show)


-- PUNTO 1 
    --- Modelar a los ratones 
cerebro :: Raton
cerebro = UnRaton "Cerebro" 9 0.2 ["Brucelosis", "Sarampión","Tuberculosis"]
bicenterrata :: Raton
bicenterrata = UnRaton "Bicenterrata" 256 0.2 []
huesudo :: Raton
huesudo = UnRaton "Huesudo" 4 10 ["Obesidad","Sinusitis"]