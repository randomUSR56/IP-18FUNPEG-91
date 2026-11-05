module ZHGyak where

import Data.Char

doubleTriple :: [a] -> [a]
doubleTriple (a:[]) = a:a:a:[]
doubleTriple (a:b:[]) = a:a:b:b:[]
doubleTriple (xs) = xs

lengthOfShorter :: [a] -> [b] -> Integer
lengthOfShorter [] _ = 0
lengthOfShorter _ [] = 0
lengthOfShorter (_:xs) (_:ys) = 1 + lengthOfShorter xs ys

compressLetters :: String -> String
compressLetters (x:y:xs)
    | x == y && isLower x = toUpper x : compressLetters xs
    | otherwise           = x : compressLetters (y:xs)
compressLetters xs = xs

areSameParity :: Integral a => (a, a, a) -> Bool
areSameParity (a, b, c)
    | ((a `mod` 2 == 0) && (b `mod` 2 == 0) && (c `mod` 2 == 0)) = True
    | ((a `mod` 2 /= 0) && (b `mod` 2 /= 0) && (c `mod` 2 /= 0)) = True
    | otherwise = False

selectElems :: [a] -> [Bool] -> [a]
selectElems [] _ = []
selectElems _ [] = []
selectElems (x:xs) (y:ys)
    | (y) = x:selectElems xs ys
    | otherwise = selectElems xs ys

haveAtLeastTwoAndThreeElems :: ([a], [b]) -> Bool
haveAtLeastTwoAndThreeElems (_:_:xs, _:_:_:ys) = True
haveAtLeastTwoAndThreeElems (_:_:_:xs, _:_:ys) = True
haveAtLeastTwoAndThreeElems _ = False

sumProduct :: Num a => [a] -> [a] -> a
sumProduct (x:xs) (y:ys) = x*y + sumProduct xs ys
sumProduct _ _ = 0

sphereVolume :: (Floating a, Ord a) => a -> a
sphereVolume r
    | r<=0 = 0
    | otherwise = (4/3)*(pi)*(r^3)

collatzFn :: Integral a => a -> a
collatzFn x
    | (x `mod` 2 /= 0) = (x*3)+1
    | otherwise = x `div` 2

sumNeighbours :: Num a => [a] -> [a]
sumNeighbours [] = []
sumNeighbours (x:[]) = []
sumNeighbours (x:y:[]) = (x+y):[]
sumNeighbours (x:y:xs) = (x+y):sumNeighbours (y:xs)

max3 :: Ord a => (a, a, a) -> a
max3 (a, b, c)
    | a >= b && a >= c = a
    | b >= c = b
    | otherwise = c