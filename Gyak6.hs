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

allOnes :: [Integer] -> Bool
allOnes ([]) = True
allOnes (1:[]) = True
allOnes (x:xs)
    | (x/=1) = False
    | otherwise = allOnes xs

allOnes' :: [Integer] -> Bool
allOnes' ([]) = True
allOnes' (1:xs) = allOnes' xs
allOnes' _ = False

numOfEmpties :: [[a]] -> Integer
numOfEmpties [] = 0
numOfEmpties ([]:xs) = 1 + numOfEmpties xs
numOfEmpties (_:xs) = numOfEmpties xs

numOfNonEmpties :: [[a]] -> Integer
numOfNonEmpties [] = 0
numOfNonEmpties ((_:_):xs) = 1 + numOfNonEmpties xs
numOfNonEmpties (_:xs) = numOfNonEmpties xs

firstTrue :: [(Bool, a)] -> [(Bool, a)]
firstTrue [] = []
firstTrue (a@(True,_):xs) = a : firstTrue xs
firstTrue (_:xs) = firstTrue xs