module Lib where
import Text.Show.Functions

laVerdad = True



-- PUNTO 1

    -- Parte a
type Recurso = String

data Pais = UnPais {
    nombre :: String,
    ingresoPC :: Float,
    poblacionSPub :: Float,
    poblacionSPri :: Float,
    recursos :: [Recurso],
    deudaFMI :: Float
} deriving (Show)

    -- Parte b

namibia = UnPais {
    nombre = "Namibia",
    ingresoPC = 4140,
    poblacionSPub = 400000,
    poblacionSPri = 650000,
    recursos = ["Minería","ecoturismo"], 
    deudaFMI = 50
} 

-- PUNTO 2

type Estrategia = Pais -> Pais

-- prestarle n millones de dólares al país, esto provoca que el país se endeude en un 150% de lo que el FMI le presta (por los intereses)

prestarleNMill :: Float -> Estrategia
prestarleNMill n = endeudarPaisPorcentual 150 n 

endeudarPaisPorcentual :: Float -> Float -> Pais -> Pais
endeudarPaisPorcentual porcentaje n pais = pais {deudaFMI = deudaFMI pais + (porcentajeReal porcentaje * n)}

porcentajeReal x = x * 0.01


-- reducir x cantidad de puestos de trabajo del sector público, lo que provoca que se reduzca la cantidad de activos en el sector público y 
-- además que el ingreso per cápita disminuya en 20% si los puestos de trabajo son más de 100 ó 15% en caso contrario

reducirPuestosSP :: Estrategia
reducirPuestosSP pais | poblacionSPub pais > 100 = efectoReducirPuestosSP 20 pais
    | otherwise = efectoReducirPuestosSP 15 pais

efectoReducirPuestosSP :: Float -> Pais -> Pais
efectoReducirPuestosSP n = disminuirIngresosPC n . disminuirPoblaciónSPub n 

disminuirPoblaciónSPub n pais = pais { poblacionSPub = poblacionSPub pais * (porcentajeReal n)}

disminuirIngresosPC n pais = pais { ingresoPC = ingresoPC pais * (porcentajeReal n) }


-- darle a una empresa afín al FMI la explotación de alguno de los recursos naturales, esto disminuye 2 millones de dólares la deuda que el 
-- país mantiene con el FMI pero también deja momentáneamente sin recurso natural a dicho país. No considerar qué pasa si el país no tiene dicho recurso.

darRN :: Recurso -> Estrategia
darRN recurso = perderRecurso recurso . disminuirDeuda 2

perderRecurso :: Recurso -> Pais -> Pais
perderRecurso recurso pais = pais {recursos = filter ( /= recurso) (recursos pais)}

disminuirDeuda :: Float -> Pais -> Pais
disminuirDeuda n pais = pais {deudaFMI = deudaFMI pais - n}


-- establecer un “blindaje”, lo que provoca prestarle a dicho país la mitad de su Producto Bruto Interno (que se calcula como el ingreso per cápita 
-- multiplicado por su población activa, sumando puestos públicos y privados de trabajo) y reducir 500 puestos de trabajo del sector público. 
-- Evitar la repetición de código.

blindaje :: Estrategia
blindaje = prestamoPBI . disminuirPoblaciónSPub' 500

prestamoPBI :: Pais -> Pais
prestamoPBI pais = undefined

pBI :: Pais -> Float
pBI pais = ingresoPC pais * poblacionActiva pais 

poblacionActiva :: Pais -> Float
poblacionActiva pais = (+ poblacionSPub pais). poblacionSPri $ pais

disminuirPoblaciónSPub' :: Float -> Pais -> Pais
disminuirPoblaciónSPub' n pais = pais {poblacionSPub = poblacionSPub pais - n}



-- PUNTO 3

    -- Modelar una receta que consista en prestar 200 millones, y darle a una empresa X la explotación de la “Minería” de un país.

type Receta = Pais -> Pais

recetaEjemplo :: Receta
recetaEjemplo = prestarleNMill 200 . darRN "Minería"

    -- Ahora queremos aplicar la receta del punto 3.a al país Namibia (creado en el punto 1.b). Justificar cómo se logra el efecto colateral.

ejemploPunto3b = recetaEjemplo namibia


-- PUNTO 4