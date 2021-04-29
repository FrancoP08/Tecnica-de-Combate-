module Spec where
import PdePreludat
import Library
import Test.Hspec

correrTests :: IO ()
correrTests = hspec $ do
-- Testeo de la Funcion "poderDelGolpe"
  describe "poderDelGolpe" $ do
    it "Multiplica por 15 las horas de entrenamiento" $ do
      poderDelGolpe 10 `shouldBe` 150
      poderDelGolpe 5.5 `shouldBe` 82.5

-- Testeo de la Funcion "fortalezaDelObjetivo"
  describe "fortalezaDelObjetivo" $ do 
    it "Multiplica por 2 la cantidad de letras del nombre del objetivo" $ do
      fortalezaDelObjetivo "bolsa de entrenamiento" `shouldBe` 44
      fortalezaDelObjetivo "mortimer" `shouldBe` 16
      
-- Testeo de la Funcion "presionDelGolpe"
  describe "presionDelGolpe" $ do
    it "Dada una cantidad de horas de entrenamiento y el nombre de un objeto devuelva la presión que ejerce el golpe sobre el objetivo" $ do
      presionDelGolpe 11 "bolsa de entrenamiento" `shouldBe` 3.75
      presionDelGolpe 44 "bolsa de entrenamiento" `shouldBe` 15

-- Testeo de la Funcion "tecnicaGomuGomu"
  describe "tecnicaGomuGomu" $ do
    it "Dadas unas 180 horas de entrenamiento y el nombre de un objeto devuelva la presión que ejerce el golpe sobre el objetivo" $ do
      tecnicaGomuGomu "luffy" `shouldBe` 270
      tecnicaGomuGomu "Homero" `shouldBe` 225

-- Testeo de la Funcion "tecnicaGolpesConsecutivos"
  describe "tecnicaGolpesConsecutivos" $ do
    it "Dadas unas 240 horas de entrenamiento y el nombre de un objeto devuelva la presión que ejerce el golpe sobre el objetivo" $ do
      tecnicaGolpesConsecutivos "deku" `shouldBe` 450
      tecnicaGolpesConsecutivos "sakura" `shouldBe` 300

-- Testeo de la Funcion "objetivoEsDificil"
  describe "objetivoEsDificil" $ do
    it "Dado el nombre de un objetivo, concluye que la presión que ejerce la técnica de gomu gomu elephant gatling sea menor a 100" $ do
      objetivoEsDificil "maggie simpson" `shouldBe` True
      objetivoEsDificil "mikasa ackerman" `shouldBe` True

    it "Dado el nombre de un objetivo, concluye que la presión que ejerce la técnica de gomu gomu elephant gatling no sea menor a 100" $ do
      objetivoEsDificil "all for one" `shouldBe` False
      objetivoEsDificil "saitama" `shouldBe` False  

-- Testeo de la Funcion "objetivoEsAccesible"
  describe "objetivoEsAccesible" $ do
    it "Dado el nombre de un objetivo, concluye que la medicion de presion que se le hace al objetivo es mayor a 200 y es menor a 400" $ do
      objetivoEsAccesible "naruto" `shouldBe` True
      objetivoEsAccesible "usopp" `shouldBe` True 

    it "Dado el nombre de un objetivo, concluye que la medicion de presion que se le hace al objetivo es mayor a 200 y no es menor a 400" $ do
      objetivoEsAccesible "ned" `shouldBe` False
      objetivoEsAccesible "zoro" `shouldBe` False  

-- Testeo de la Funcion "medicionDelObjetivo"
  describe "medicionDelObjetivo" $ do
    it "Dado el nombre de un objetivo, calcula la presion que ejerce la tecnica de golpes consecutivos normales al olbjetivo focalizado" $ do
      medicionDelObjetivo "kaido" `shouldBe` 360
      medicionDelObjetivo "alf" `shouldBe` 600

-- Testeo de la Funcion "objetivoFocalizado"
  describe "objetivoFocalizado" $ do
    it "Dado el nombre de un objetivo, focaliza al objetivo calculando los primeros 7 caracteres del nombre" $ do
      objetivoFocalizado "majin boo" `shouldBe` "majin b"
      objetivoFocalizado "sanji" `shouldBe` "sanji"
  