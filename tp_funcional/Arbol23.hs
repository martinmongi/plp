module Arbol23 where

data Arbol23 a b = Hoja a
                 | Dos  b   (Arbol23 a b) (Arbol23 a b) 
                 | Tres b b (Arbol23 a b) (Arbol23 a b) (Arbol23 a b)

{- Funciones para mostrar el árbol. -}

instance (Show a, Show b) => Show (Arbol23 a b) where
    show = ("\n" ++) . (padTree 0 0 False)

padlength :: Int
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

stuff :: Show a => a -> String
stuff x = if n > l then pad (n-l) ++ s else s
  where s = show x
        l = length s
        n = padlength

pad :: Int -> String
pad i = replicate i ' '

{- Funciones pedidas. -}

foldA23 :: (a -> c) -> (b -> c -> c -> c) -> (b -> b -> c -> c -> c -> c) -> Arbol23 a b -> c
foldA23 fHoja fDos fTres t = case t of
    Hoja x              -> fHoja x
    Dos  k r1 r2        -> fDos  k     (f r1) (f r2)
    Tres k1 k2 r1 r2 r3 -> fTres k1 k2 (f r1) (f r2) (f r3)
  where f = foldA23 fHoja fDos fTres

--Lista en preorden de los internos del árbol.
internos :: Arbol23 a b -> [b]
internos = foldA23 (const []) fDos fTres
  where fDos  k     x y   = [k]      ++ x ++ y
        fTres k1 k2 x y z = [k1, k2] ++ x ++ y ++ z

--Lista las hojas de izquierda a derecha.
hojas :: Arbol23 a b -> [a]
hojas = foldA23 (:[]) fDos fTres
  where fDos  _   x y   = x ++ y
        fTres _ _ x y z = x ++ y ++ z

-- True si el arbol es hoja, falso si es Dos o Tres
esHoja :: Arbol23 a b -> Bool
esHoja a = case a of
  Hoja _    -> True
  otherwise -> False

-- Map de un Arbol23 a otro
mapA23 :: (a -> c) -> (b -> d) -> Arbol23 a b -> Arbol23 c d
mapA23 fH fI = foldA23 mHoja mDos mTres
  where mHoja x              = Hoja (fH x)
        mDos  k     r1 r2    = Dos  (fI k)          r1 r2
        mTres k1 k2 r1 r2 r3 = Tres (fI k1) (fI k2) r1 r2 r3

--Ejemplo de uso de mapA23.
--Incrementa en 1 el valor de las hojas.
incrementarHojas :: Num a => Arbol23 a b -> Arbol23 a b
incrementarHojas = mapA23 (+1) id

-- aplica sucesivamente n veces la funcion f, con neutro z.
foldNat :: a -> (a -> a) -> Integer -> a
foldNat z f n = case n of
  0 -> z
  _ -> f (foldNat z f (n-1))

--Trunca el árbol hasta un determinado nivel. Cuando llega a 0, reemplaza el
--resto del árbol por una hoja con el valor indicado.
--Funciona para árboles infinitos.
truncar :: a -> Integer -> (Arbol23 a b -> Arbol23 a b)
truncar z = foldNat (const (Hoja z)) crecer

-- dada f, devuelve una funcion que aplica f a los subarboles de un nodo
crecer :: (Arbol23 a b -> Arbol23 a b) -> (Arbol23 a b -> Arbol23 a b)
crecer f = foldA23 Hoja fDos fTres
  where fDos  x   r1 r2    = Dos  x   (f r1) (f r2)
        fTres x y r1 r2 r3 = Tres x y (f r1) (f r2) (f r3)

-- truncar pero mucho más rápido. testeado con truncar' 0 20 arbolito3'
truncar' :: a -> Integer -> Arbol23 a b -> Arbol23 a b
truncar' z n a = hastaNivel z n (conNiveles a)

-- decora un arbol con su numero de nivel, la raiz es 1, el primer nivel es 2, etc.
conNiveles :: Arbol23 a b -> Arbol23 (a, Integer) (b, Integer)
conNiveles = foldA23 h d t
  where h x            =         Hoja (z x)
        d x   r1 r2    = mapInc (Dos  (z x)       r1 r2)
        t x y r1 r2 r3 = mapInc (Tres (z x) (z y) r1 r2 r3)
        z x = (x, 0)
        mapInc = mapA23 inc inc
        inc (x, n) = (x, n+1)

-- devuelve el arbol decorado hasta el nivel indicado, reemplazando el nivel cortado por (Hoja z)
hastaNivel :: a -> Integer -> (Arbol23 (a, Integer) (b, Integer) -> Arbol23 a b)
hastaNivel z n = foldA23 h d t
  where h (x, xn)                 = f xn (Hoja x)             z'
        d (x, xn)        r1 r2    = f xn (Dos  x   r1 r2)    (Dos  x   z' z')
        t (x, xn) (y, _) r1 r2 r3 = f xn (Tres x y r1 r2 r3) (Tres x y z' z' z')
        f nivel orig trunco = if nivel < n then orig else trunco
        z' = Hoja z

--Evalúa las funciones tomando los valores de los hijos como argumentos.
--En el caso de que haya 3 hijos, asocia a izquierda.
evaluar :: Arbol23 a (a -> a -> a) -> a
evaluar = foldA23 id (\ f a b -> f a b) (\ f g a b c -> g (f a b) c)

--Ejemplo:
--evaluar (truncar 0 6 arbolito3) = 22 = (1*2-3)+(2*3-4)+(3*4-5)+(4*5-6)

{- Árboles de ejemplo. -}
arbolito1 :: Arbol23 Char Int
arbolito1 = Tres 0 1
        (Dos 2 (Hoja 'a') (Hoja 'b'))
        (Tres 3 4 (Hoja 'c') (Hoja 'd') (Dos 5 (Hoja 'e') (Hoja 'f')))
        (Dos 6 (Hoja 'g') (Dos 7 (Hoja 'h') (Hoja 'i')))

arbolito2 :: Arbol23 Int Bool
arbolito2 = Dos True (Hoja (-1)) (Tres False True (Hoja 0) (Hoja (-2)) (Hoja 4))

arbolito3 :: Arbol23 Int (Int->Int->Int)
arbolito3 = Dos (+) (Tres (*) (-) (Hoja 1) (Hoja 2) (Hoja 3)) (incrementarHojas arbolito3)

arbolito3' :: Arbol23 Int String
arbolito3' = Dos "(+)" (Tres "(*)" "(-)" (Hoja 1) (Hoja 2) (Hoja 3)) (incrementarHojas arbolito3')

arbolito4 :: Arbol23 Int Char
arbolito4 = Dos 'p' (Dos 'l' (Dos 'g' (Hoja 5) (Hoja 2)) (Tres 'r' 'a' (Hoja 0)(Hoja 1)(Hoja 12)))
                    (Dos 'p' (Tres 'n' 'd' (Hoja (-3))(Hoja 4)(Hoja 9)) (Dos 'e' (Hoja 20)(Hoja 7)))
