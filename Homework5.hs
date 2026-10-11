module Homework5 where
import Control.Arrow (Arrow(second))

signOfHead :: [Integer] -> Integer
signOfHead (x:_)
    | (x>0) = 1
    | (x<0) = (-1)
    | otherwise = 0

secondLast :: [a] -> a
secondLast (x:y:[]) = x
secondLast (_:xs) = secondLast xs

{-
last' :: [a] -> a
last' (x:[]) = x
last' (_:xs) = last' xs
-}

isLongerThan :: [a] -> Integer -> Bool
isLongerThan [] y
    | y < 0 = True
    | otherwise = False
isLongerThan (_:xs) y
    | (y<=0) = True
    |  otherwise = isLongerThan xs (y-1)

numOfOcc :: Eq a => a -> [a] -> Integer
numOfOcc _ [] = 0
numOfOcc y (x:xs)
    | (y /= x) = (numOfOcc y xs) + 0
    | otherwise = (numOfOcc y xs) + 1
