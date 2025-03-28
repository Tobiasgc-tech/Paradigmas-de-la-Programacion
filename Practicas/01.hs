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
evaluarEnCero = (Float -> Float) -> Float

dosVeces = \f -> f . f
:t dosVeces = (a -> a) -> a -> a

flipAll = map flip
:t flipAll = [(a -> b -> c)] -> [(b -> a -> c)]

flipRaro = flip flip
t: flipRaro = ((a -> b -> c) -> (b -> a -> c)) -> ((b -> a -> c) -> (a -> b -> c))
-}

--Ejercicio2

--Ejercicio3
sumFoldr:: [Int] -> Int
sumFoldr xs = foldr (+) 0 xs

elemFoldr:: Int -> [Int] -> Bool
elemFoldr elemento xs = foldr (\x res -> x == elemento || res) False xs

concatFoldr:: [a] -> [a] -> [a]
concatFoldr xs ys = foldr (:) ys xs

filterFoldr:: (a -> Bool) -> [a] -> [a]
filterFoldr f xs = foldr (\x r -> if f x then x:r else r) [] xs

mapFoldr:: (a -> b) -> [a] -> [b]
mapFoldr f xs = foldr (\x r -> f x :r) [] xs