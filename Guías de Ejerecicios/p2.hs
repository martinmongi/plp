max2 (x, y) | x >= y = x
            | otherwise = y

normaVectorial (x, y) = sqrt (x^2 + y^2)

subtract2 = flip (-)

predecesor = subtract 1

evaluarEnCero = \f -> f 0

dosVeces = \f -> f.f

flipAll = map flip

flipRaro = flip flip

curry2 f x y = f (x, y)
uncurry2 f (x,y) = f x y

pitagoricas :: [(Integer, Integer, Integer)]
pitagoricas = [(a, b, c) | c <- [1..], b <-[1..c], a <- [1..b], a^2 + b^2 == c^2]
