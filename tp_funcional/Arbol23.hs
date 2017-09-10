module Arbol23 where

data Arbol23 a b = Hoja a | Dos b (Arbol23 a b) (Arbol23 a b) | Tres b b (Arbol23 a b) (Arbol23 a b) (Arbol23 a b)

{- Funciones para mostrar el árbol. -}

instance (Show a, Show b) => Show (Arbol23 a b) where
    show = ("\n" ++) . (padTree 0 0 False)

padlength::Int
padlength = 5

padTree:: (Show a, Show b) => Int -> Int -> Bool -> (Arbol23 a b)-> String
padTree nivel acum doPad t = case t of
				  (Hoja x) -> initialPad ++ stuff x
                                  (Dos x i d) -> initialPad ++ stuff x ++
                                                 pad padlength ++ rec x False i ++ "\n" ++
                                                 rec x True d ++ "\n"
                                  (Tres x y i m d) -> initialPad ++ stuff x ++ --(' ':tail (stuff y)) ++
                                                      pad padlength ++ rec x False i ++ "\n" ++
                                                      pad levelPad ++ stuff y ++ pad padlength ++ rec x False m ++ "\n" ++
                                                      rec x True d ++ "\n"
  where l = length . stuff
	levelPad = (padlength*nivel + acum)
	initialPad = (if doPad then pad levelPad else "")
	rec x = padTree (nivel+1) (acum+l x)

stuff:: Show a => a -> String
stuff x = if n > l then pad (n-l) ++ s else s
  where s = show x
        l = length s
        n = padlength

pad:: Int -> String
pad i = replicate i ' '

{- Funciones pedidas. -}

foldA23::(dato->res) -> (clave->res->res->res) -> (clave->clave->res->res->res->res) -> Arbol23 dato clave -> res
foldA23 f1 f2 f3 (Hoja x) = f1 x
foldA23 f1 f2 f3 (Dos k r1 r2) = f2 k (foldA23 f1 f2 f3 r1) (foldA23 f1 f2 f3 r2)
foldA23 f1 f2 f3 (Tres k1 k2 r1 r2 r3) = f3 k1 k2 (foldA23 f1 f2 f3 r1) (foldA23 f1 f2 f3 r2) (foldA23 f1 f2 f3 r3)

--Lista en preorden de los internos del árbol.

aux_internos_f1::dato->[clave]
aux_internos_f1 _ = []

aux_internos_f2::clave->[clave]->[clave]->[clave]
aux_internos_f2 k x y = [k] ++ x ++ y

aux_internos_f3::clave->clave->[clave]->[clave]->[clave]->[clave]
aux_internos_f3 k1 k2 x y z = [k1,k2] ++ x ++ y ++ z

internos::Arbol23 dato clave->[clave]
internos x = foldA23 aux_internos_f1 aux_internos_f2 aux_internos_f3 x

--Lista las hojas de izquierda a derecha.
aux_hojas_f1::dato->[dato]
aux_hojas_f1 x = [x]

aux_hojas_f2::clave->[dato]->[dato]->[dato]
aux_hojas_f2 _ x y = x ++ y

aux_hojas_f3::clave->clave->[dato]->[dato]->[dato]->[dato]
aux_hojas_f3 _ _ x y z = x ++ y ++ z

hojas::Arbol23 dato clave->[dato]
hojas x = foldA23 aux_hojas_f1 aux_hojas_f2 aux_hojas_f3 x

esHoja::Arbol23 dato clave->Bool
esHoja (Hoja _) = True
esHoja _ = False

aux_mapA23_f1::(dato1->dato2)->dato1->Arbol23 dato2 clave2
aux_mapA23_f1 f x = Hoja (f x)

aux_mapA23_f2::(clave1->clave2)->clave1->Arbol23 dato2 clave2->Arbol23 dato2 clave2->Arbol23 dato2 clave2
aux_mapA23_f2 f k r1 r2 = Dos (f k) r1 r2

aux_mapA23_f3::(clave1->clave2)->clave1->clave1->Arbol23 dato2 clave2->Arbol23 dato2 clave2->Arbol23 dato2 clave2->Arbol23 dato2 clave2
aux_mapA23_f3 f k1 k2 r1 r2 r3 = Tres (f k1) (f k2) r1 r2 r3

mapA23::(dato1->dato2)->(clave1->clave2)->Arbol23 dato1 clave1->Arbol23 dato2 clave2
mapA23 fdato fclave r = foldA23 (aux_mapA23_f1 fdato) (aux_mapA23_f2 fclave) (aux_mapA23_f3 fclave) r

--Ejemplo de uso de mapA23.
--Incrementa en 1 el valor de las hojas.
incrementarHojas::Num a =>Arbol23 a b->Arbol23 a b
incrementarHojas = mapA23 (+1) id

--Trunca el árbol hasta un determinado nivel. Cuando llega a 0, reemplaza el resto del árbol por una hoja con el valor indicado.
--Funciona para árboles infinitos.
truncar::a->Integer->Arbol23 a b->Arbol23 a b
truncar = undefined

--Evalúa las funciones tomando los valores de los hijos como argumentos.
--En el caso de que haya 3 hijos, asocia a izquierda.
evaluar::Arbol23 a (a->a->a)->a
evaluar = undefined

--Ejemplo:
--evaluar (truncar 0 6 arbolito3) = 22 = (1*2-3)+(2*3-4)+(3*4-5)+(4*5-6)

{- Árboles de ejemplo. -}
arbolito1::Arbol23 Char Int
arbolito1 = Tres 0 1
        (Dos 2 (Hoja 'a') (Hoja 'b'))
        (Tres 3 4 (Hoja 'c') (Hoja 'd') (Dos 5 (Hoja 'e') (Hoja 'f')))
        (Dos 6 (Hoja 'g') (Dos 7 (Hoja 'h') (Hoja 'i')))

arbolito2::Arbol23 Int Bool
arbolito2 = Dos True (Hoja (-1)) (Tres False True (Hoja 0) (Hoja (-2)) (Hoja 4))

arbolito3::Arbol23 Int (Int->Int->Int)
arbolito3 = Dos (+) (Tres (*) (-) (Hoja 1) (Hoja 2) (Hoja 3)) (incrementarHojas arbolito3)

arbolito4::Arbol23 Int Char
arbolito4 = Dos 'p' (Dos 'l' (Dos 'g' (Hoja 5) (Hoja 2)) (Tres 'r' 'a' (Hoja 0)(Hoja 1)(Hoja 12)))
                    (Dos 'p' (Tres 'n' 'd' (Hoja (-3))(Hoja 4)(Hoja 9)) (Dos 'e' (Hoja 20)(Hoja 7)))
