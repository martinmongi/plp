import Diccionario
import Data.Maybe
import Data.List
import Arbol23
import Test.HUnit

--Este módulo sirve para utilizar el diccionario sin acceder a su estructura interna. Pueden agregar otras funciones o casos de prueba.

{- Función a implementar. -}

búsquedaDelTesoro :: Eq a => a -> (a->Bool) -> Diccionario a a -> Maybe a
búsquedaDelTesoro p f d = until nadaOTesoro buscarProximaPista (Just p)
  where
    nadaOTesoro = maybe True f
    buscarProximaPista = maybe Nothing (flip obtener $ d)

{- Diccionarios de prueba: -}

dicc1::Diccionario Int String
dicc1 = definirVarias [(0,"Hola"),(-10,"Chau"),(15,"Felicidades"),(2,"etc."),(9,"a")] (vacio (<))

dicc2::Diccionario String String
dicc2 = definirVarias [("inicio","casa"),("auto","flores"),("calle","auto"),("casa","escalera"),("ropero","alfajor"),("escalera","ropero")] (vacio (<))

dicc3::Diccionario Int String
dicc3 = definirVarias [(0,"Hola"),(-10,"Chau"),(15,"Felicidades"),(2,"etc."),(9,"a")] (vacio (\x y->x `mod` 5 < y `mod` 5))

--Ejecución de los tests
main :: IO Counts
main = do runTestTT allTests

allTests = test [
  "ejercicio2" ~: testsEj2,
  "ejercicio3" ~: testsEj3,
  "ejercicio4" ~: testsEj4,
  "ejercicio5" ~: testsEj5,
  "ejercicio6" ~: testsEj6,
  "ejercicio7" ~: testsEj7,
  "ejercicio8" ~: testsEj8,
  "ejercicio9" ~: testsEj9,
  "ejercicio10" ~: testsEj10
  ]

testsEj2 = test [
  [0,1,2,3,4,5,6,7] ~=? internos arbolito1,
  "abcdefghi" ~=? hojas arbolito1,
  [True,False,True] ~=? internos arbolito2,
  [1,2,3,2,3,4,3,4,5,4] ~=? take 10 (hojas arbolito3)
  ]

testsEj3 = test [
  [0,1,-1,5] ~=? hojas (incrementarHojas arbolito2)
  ]

testsEj4 = test [
  [1,2,3,2,3,4,3,4,5,4,5,6,0,0,0,0,0] ~=? hojas (truncar 0 6 arbolito3),
  [0, 0, 0, 0, 0, 0, 0, 0, 0, 0] ~=? hojas (truncar 0 3 arbolito4)
  ]

testsEj5 = test [
  22 ~=? evaluar (truncar 0 6 arbolito3)
  ]

diccVacio::Diccionario Int String
diccVacio = vacio (<)

testsEj6 = test [
  True ~=? isNothing (estructura diccVacio),
  [] ~=? claves diccVacio,
  Nothing ~=? obtener 42 diccVacio
  ]

testsEj7 = test [
  isNothing (estructura (definir 1 2 (vacio (\x-> \y-> False)))) ~=? False,
  isJust (estructura (definir 1 2 (vacio (\x-> \y-> False)))) ~=? True,
  "perro" ~=? fromJust (obtener 1 (definir 1 "perro" diccVacio))
  ]

testsEj8 = test [
  "Hola" ~=? fromJust (obtener 0 dicc1),
  "Chau" ~=? fromJust (obtener (-10) dicc1),
  "Felicidades" ~=? fromJust (obtener 15 dicc1),
  True ~=? isNothing (obtener 27 dicc1)
  ]

testsEj9 = test [
  [-10,0,2,9,15] ~=? sort(claves dicc1)
  ]

testsEj10 = test [
  Just "alfajor" ~=? búsquedaDelTesoro "inicio" ((=='a').head) dicc2,
  Nothing ~=? búsquedaDelTesoro "auto" (=="alfajor") dicc2,
  Just "alfajor" ~=? búsquedaDelTesoro "ropero" ((=='a').head) dicc2
  ]
