module Gyak4 where
import Data.Char (toUpper)

isEmpty :: [a] -> Bool
isEmpty [] = True
isEmpty _ = False

isEmpty' :: [a] -> Bool
isEmpty' (x:_) = False
isEmpty' _ = True

isSingleton :: [a] -> Bool
isSingleton (_:[]) = True -- [_]

isDoubleton :: [a] -> Bool
isDoubleton (_:_:[]) = True
isDoubleton _ = False

hd :: [a] -> a
hd (x:_) = x

tl :: [a] -> [a]
tl (_:xs) = xs

toUpperFirst :: String -> String
toUpperFirst (x:xs) = toUpper x : xs
toUpperFirst "" = ""

myProduct :: Num a => [a] -> a
myProduct [] = 1
myProduct (x:xs) = x * myProduct xs

myLast :: [a] -> a
myLast (x:[]) = x
myLast (_:xs) = myLast xs

myMinimum :: [Integer] -> Integer
myMinimum (x:[]) = x
myMinimum (x:xs) = x `min` myMinimum xs

infixl 9 !:!
(!:!) :: [a] -> Int -> a
(!:!) (x:xs) i
    | i == 0 = x
    | i > 0 = xs !:! (i-1)