module Gyak6 where

descending :: Integer -> [Integer]
descending k = [k, (k-1)..(-k)]

divByN :: Integer -> [Integer]
divByN n
    | (n<=0) = []
    | otherwise = [0, n..]

fst' :: (a, b) -> a
fst' (x,_) = x

h :: [Bool] -> Char
h (x:False:xs) = 'h'

-- i :: [[a]] -> Char
-- i ([]:xs) -> 'i'

f :: (Eq a, Num a) => [a] -> Char
f (1:x:xs) = 'a'
f (x:2:xs) = 'b'
f (1:[])   = 'c'
f _        = 'd'