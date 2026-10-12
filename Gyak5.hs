module Gyak5 where
import Data.Char (isUpper)

myDrop :: Int -> [a] -> [a]
myDrop n xs
    | n <= 0 = xs
myDrop _ [] = []
myDrop n (_:xs) = myDrop (n-1) xs

isAscending :: Ord a => [a] -> Bool
isAscending (x:y:xs) = x<y && isAscending (y:xs)
isAscending _ = True

myTake :: Int -> [a] -> [a]
myTake _ [] = []
myTake n (x:xs)
    | (n<=0) = []
    | otherwise = x : myTake (n-1) xs

myReplicate :: Int -> a -> [a]
myReplicate n e
    | n <= 0 = []
    | otherwise = e : myReplicate (n-1) e

onlyUppers :: String -> String
onlyUppers "" = ""
onlyUppers (x:xs)
    | isUpper x = x : onlyUppers xs
    | otherwise = onlyUppers xs

infixr 5 +:+
(+:+) :: [a] -> [a] -> [a]
[] +:+ ys = ys
(x:xs) +:+ ys = x : (xs +:+ ys)

injectElem :: a -> [a] -> [a]
injectElem elem (x:y:xs) = x:elem: injectElem elem (y:xs)
injectElem elem xs = xs

sortedMerge :: Ord a => [a] -> [a] -> [a]
sortedMerge [] ys = ys
sortedMerge xs [] = xs
sortedMerge (x:xs) (y:ys)
    | x<y = x : sortedMerge xs (y:ys)
    | x>=y = y : sortedMerge (x:xs) ys