module Homework4 where

-- 1. feladat
-- Valid (True), ha az elso parameter 0-23, a masodik pedig 0-59 koze esik.
-- Egyebkent nem (False)
validTime :: Integer -> Integer -> Bool
validTime a b
    | ((0<=a) && (a<=23)) && ((0<=b) && (b<=59)) = True
    | otherwise = False

-- 2. feladat
-- main
pwr :: Integer -> Integer -> Integer
pwr x n
    | n==0 = 1
    | (n `mod` 2 == 1) = x*(pwr x (n-1))
    | otherwise = sqr (pwr x (n `div` 2))
    where -- helper
        sqr :: Integer -> Integer
        sqr y = y * y

-- 3. feladat
-- main
sumOfDivisors :: Integer -> Integer
sumOfDivisors x -- x: szam aminek szeretnem az osztoinak osszeget
    | (x<=0) = 0
    | otherwise = sumDivisorsUpTo x
    where --helper
        sumDivisorsUpTo :: Integer -> Integer
        sumDivisorsUpTo b -- b: oszto-jelolt
            | (b == 0) = 0
            | (x `mod` b /= 0) = sumDivisorsUpTo (b-1)
            | otherwise = b + sumDivisorsUpTo (b-1)

-- fibonacci
fibonacci :: Integer -> Integer
fibonacci n
    | (n == 0) = 0
    | (n == 1) = 1
    | otherwise = fibonacci(n-1) + fibonacci (n-2)