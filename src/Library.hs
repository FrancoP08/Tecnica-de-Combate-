module Library where
import PdePreludat
type Poder = Number  

double :: Number -> Number
double = (*2)

type Horas = Number

poderDelGolpe :: Horas -> Number
poderDelGolpe = (*15) 

type Objetivo = String

fortalezaDelObjetivo :: Objetivo -> Number
fortalezaDelObjetivo = (*2).length

presionDelGolpe :: Horas -> Objetivo -> Number
presionDelGolpe horasDeEntrenamiento nombreDelObjetivo = poderDelGolpe horasDeEntrenamiento / fortalezaDelObjetivo nombreDelObjetivo



type Golpe = Objetivo -> Number

tecnicaGomuGomu :: Golpe 
tecnicaGomuGomu = presionDelGolpe 180 

tecnicaGolpesConsecutivos :: Golpe
tecnicaGolpesConsecutivos = presionDelGolpe 240



objetivoEsDificil :: Objetivo -> Bool
objetivoEsDificil nombreDelObjetivo = (tecnicaGomuGomu nombreDelObjetivo) < 100 

objetivoEsAccesible :: Objetivo -> Bool
objetivoEsAccesible nombreDelObjetivo = medicionDelObjetivo nombreDelObjetivo >= 200 && medicionDelObjetivo nombreDelObjetivo <= 400

medicionDelObjetivo :: Golpe
medicionDelObjetivo = tecnicaGolpesConsecutivos.objetivoFocalizado

type Focalizar = Objetivo -> String

objetivoFocalizado :: Focalizar
objetivoFocalizado = take 7 


