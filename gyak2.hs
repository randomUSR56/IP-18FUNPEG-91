module Gyak2 where
import Distribution.Simple.Utils (xargs)
-- import Gyak1 (isA)

isA :: Char -> Bool
isA 'a' = True
isA 'A' = True
isA _ = False

double :: Integer -> Integer
double x = 2*x

isEven :: Integer -> Bool
isEven x = x `mod` 2 == 0

isOdd :: Integer -> Bool
-- isOdd y = not (isEven y)
isOdd y = y `mod` 2 /= 0

divides :: Integer -> Integer -> Bool
divides n m = m `mod` n == 0

triangleSides :: Integer -> Integer -> Integer -> Bool
triangleSides a b c = (a + b) > c && (a + c) > b && (b + c) > a

isLeapYear :: Integer -> Bool
isLeapYear x = ((x `mod` 4 == 0) && (x `mod` 100 /= 0)) || (x `mod` 400 == 0)

isZero :: Integer -> Bool
isZero 0 = True
isZero _ = False

mul3 :: Int -> Int -> Int
mul3 0 _ = 0
mul3 _ 0 = 0
mul3 1 x = x
mul3 x 1 = x
mul3 2 2 = 1 -- mul _ _ = 1

mul3' :: Int -> Int -> Int
mul3' x y = x * y `mod` 3

-- rekurzio
fact :: Integer -> Integer
fact 0 = 1
fact n = n * fact (n-1)