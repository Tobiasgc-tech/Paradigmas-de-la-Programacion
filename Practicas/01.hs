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

mejorSegun:: (a -> a -> Bool) -> [a] -> a
mejorSegun f = foldl1 (\x y -> if f x y then x else y)

{-sumasParciales:: Num a => [a] -> [a]
sumasParciales = foldl (\x acc -> (x + if null acc then 0 else head acc) : acc) []-}

{-sumaAlt :: [Int] -> Int
sumaAlt [] = 0
sumaAlt (x:xs) = if even(length (x:xs))
                 then sumaAlt xs - x
                 else sumaAlt xs + x -}

sumaAlt :: [Int] -> Int
sumaAlt =fst.foldr (\x (acc, i) -> if even i
                                   then (acc + x , i+1)
                                   else (acc - x ,i+1))
                                   (0,0)

--Ejercicio5
entrelazar :: [a] -> [a] -> [a]
entrelazar = foldr (\x acc ys -> if null ys
                                 then x : acc [] 
                                 else x : head ys : acc (tail ys)) id

--Ejercicio6
recr :: (a -> [a] -> b -> b) -> b -> [a] -> b
recr _ z [] = z
recr f z (x : xs) = f x xs (recr f z xs)

sacarUna :: Eq a => a -> [a] -> [a]
sacarUna elem = recr (\x xs acc -> if elem == x
                                   then xs
                                   else x : acc) []

{-sacarUna1 :: Eq a => a -> [a] -> [a]
sacarUna1 _ [] = []
sacarUna1 elem (x:xs) = if elem == x      
                        then xs
                        else x : sacarUna1 elem xs-}

insertarOrdenado :: Ord a => a -> [a] -> [a]
insertarOrdenado elem = recr (\x xs acc -> if elem <= x
                                           then elem : x : xs
                                           else x : acc) [elem]

--Ejercicio7
mapPares :: (a -> b -> c) -> [(a,b)] -> [c]
mapPares f = map (uncurry f)