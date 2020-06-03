module Lib where
import Text.Show.Functions

laVerdad = True

------------------------------------------------------------------------------------------------------------------------------

filtrarPalabrasTerminadasEn :: String -> [String] -> [String]
filtrarPalabrasTerminadasEn terminacion = filter (not.(palabraTerminadaEn terminacion))

palabraTerminadaEn :: String -> String -> Bool
palabraTerminadaEn terminacion palabra =  ( drop (ultimosNDigitos terminacion palabra) palabra ) == terminacion

ultimosNDigitos :: String -> String -> Int
ultimosNDigitos terminacion palabra = (length palabra - length terminacion) 

------------------------------------------------------------------------------------------------------------------------------
