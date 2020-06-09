module Lib where
import Text.Show.Functions

laVerdad = True



-- PUNTO 1

    -- Parte a
type Recurso = String

data Pais = UnPais {
    nombre :: String,
    ingresoPC :: Float,
    poblacionSPub :: Int,
    poblacionSPri :: Int,
    recursos :: [Recurso],
    deudaFMI :: Float
}

    -- Parte b

namibia = UnPais {
    nombre = "Namibia",
    ingresoPC = 4140,
    poblacionSPub = 400000,
    poblacionSPri = 650000,
    recursos = ["mineria","ecoturismo"], 
    deudaFMI = 50
}

