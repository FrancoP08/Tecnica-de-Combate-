module Library where
import PdePreludat
 
  
-- Parte 1 del Ejercicio 


presionDelGolpe :: Horas -> Objetivo -> Number
presionDelGolpe horasDeEntrenamiento nombreDelObjetivo = poderDelGolpe horasDeEntrenamiento / fortalezaDelObjetivo nombreDelObjetivo

type Horas = Number

poderDelGolpe :: Horas -> Number
poderDelGolpe = (*15) 

type Objetivo = String

fortalezaDelObjetivo :: Objetivo -> Number
fortalezaDelObjetivo = (*2).length


-- Parte 2 del Ejercicio 


type Golpe = Objetivo -> Number

tecnicaGomuGomu :: Golpe 
tecnicaGomuGomu = presionDelGolpe 180 

tecnicaGolpesConsecutivos :: Golpe
tecnicaGolpesConsecutivos = presionDelGolpe 240


-- Parte 3 del Ejercicio 


objetivoEsDificil :: Objetivo -> Bool
objetivoEsDificil nombreDelObjetivo = (tecnicaGomuGomu nombreDelObjetivo) < 100 

objetivoEsAccesible :: Objetivo -> Bool
objetivoEsAccesible nombreDelObjetivo = medicionDelObjetivo nombreDelObjetivo >= 200 && medicionDelObjetivo nombreDelObjetivo <= 400

medicionDelObjetivo :: Golpe
medicionDelObjetivo = tecnicaGolpesConsecutivos.objetivoFocalizado

type Focalizar = Objetivo -> String

objetivoFocalizado :: Focalizar
objetivoFocalizado = take 7 


