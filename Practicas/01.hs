--Ejercicio1
{-
max2 (x, y) | x >= y = x
            | otherwise = y
:t max2 = (Float, Float) -> Float
currificada
max2 Float -> Float -> Float
max2 x y | x >= x =x
         | otherwise = y

subtract = flip (-)
:t subtract = flip (-): Float -> Float -> Float

predecesor = subtract 1
:t predecesor: Float -> Float

evaluarEnCero = \f -> f 0
-}