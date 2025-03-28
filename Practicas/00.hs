--Ejercicio1
{-
null: es de tipo Foldable t => t a -> Bool, verifica si esta vacio
head: es de tipo [a] -> a, devuelve el primer elemento de la lista
tail: es de tipo [a] -> [a], devuelve una lista sin el primer elemento
init: es de tipo [a] -> [a], devuelve una lista sin el ultimo elemento
last: es de tipo [a] -> a, devuelve el ultimo elemento de una lista
take: es de tipo Int -> [a] -> [a], devuelve una lista hasta el numero n
drop: es de tipo Int -> [a] -> [a], devuelde una lista desde el numero n
(++): es de tipo [a] -> [a] -> [a], concatena dos lista
concat: es de tipo [a] -> [a], concatena la listas que estan adentro
-}
--Ejercicio2
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Use record patterns" #-}
valorAbsoluto:: Int -> Int
valorAbsoluto x | x >= 0 = x
                | otherwise = -x

bisiesto:: Int -> Bool
bisiesto x = (x `mod`4 == 0 && x `mod` 100 /= 0) || (x `mod` 400 == 0)

esPrimo:: Int -> Bool
esPrimo n = length [x | x <- [1..n], n `mod` x == 0] == 2

divisores :: Int -> [Int]
divisores n = [x | x <- [1..n], n `mod` x == 0]

cantDivisoresPrimos:: Int -> Int
cantDivisoresPrimos n = length [x | x <- divisores n, esPrimo x]

--Ejercicio3
inverso:: Float -> Maybe Float
inverso x | x == 0 = Nothing
          | otherwise = Just (1/x)

aEntero:: Either Int Bool -> Int
aEntero (Left x) = x
aEntero (Right b) = if b then 1 else 0

--Ejercicio4
limpiar:: String -> String -> String
limpiar xs ys = filter (`notElem` xs) ys

difPromedio :: [Float] -> [Float]
difPromedio [] = []  
difPromedio xs = map (difConPromedio promedio) xs
  where
    promedio = sum xs / fromIntegral (length xs)

difConPromedio :: Float -> Float -> Float
difConPromedio promedio x = x - promedio

--Ejercicio5
data AB a = Nil | Bin (AB a) a (AB a)

vacioAB :: AB a -> Bool
vacioAB Nil = True
vacioAB (Bin _ _ _) = False

negacionAb :: AB Bool -> AB Bool
negacionAb Nil = Nil
negacionAb (Bin izq valor der) = Bin (negacionAb izq) (not valor) (negacionAb der)

productoAB :: AB Int -> Int
productoAB Nil = 1
productoAB (Bin izq valor der) = valor * productoAB izq * productoAB der