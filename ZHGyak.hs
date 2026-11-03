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