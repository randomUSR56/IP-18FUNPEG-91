module Gyak7 where

-- list comprehension
squares :: [Integer]
squares = [n^2 | n <- [0..10]]

evens :: [Integer]
evens = [n | n <- [1..10], n `mod` 2 == 0]

powersOf2 :: Int -> [Integer]
powersOf2 n = [2^n | n <- [0..n]]

oddEven :: Int -> [Bool]
oddEven 0 = []
oddEven n = [n `mod` 2 == 0 | n <- [1..n]]

myReplicate :: Int -> a -> [a]
myReplicate n e = [e | _ <- [1..n]]

powerOfTwo :: Integer -> Integer
powerOfTwo n = head [2^x | x <- [0..], 2^x > n]

divBySix :: [Integer]
divBySix = [n | n <- [1..30], n `mod` 2 == 0, n `mod` 3 == 0]

descartes :: [(Integer, Integer)]
descartes = [(a,b) | a <- [1,2], b <- [3,4,5]]

time :: [(Int, Int)]
time = [(h, m) | h <- [0, 1..23], m <- [0,1..59]]

dominoes :: Integer -> [(Integer, Integer)]
dominoes n = [(a, b) | a <- [0..n], b <- [0..n], a<=b]

dominos :: Integer -> [(Integer, Integer)]
dominos n = [(a, b) | a <- [0..n], b <- [a..n]]

-- zip
-- concat
-- words
-- unwords

numOfAs :: String -> Int
numOfAs str = length [x | x <- str, x == 'a']

numOfAs' :: String -> Int
numOfAs' str = sum [1 | 'a' <- str]

-- végrekurzió

length' :: [a] -> Int
length' xs = lengthHelper xs 0
    where
        lengthHelper :: [a] -> Int -> Int
        lengthHelper [] n = n
        lengthHelper (x:xs) n = lengthHelper xs (n+1)

reverse' :: [a] -> [a]
reverse' xs = reverseHelper xs []
    where
        reverseHelper :: [a] -> [a] -> [a]
        reverseHelper (x:xs) l = reverseHelper xs (x:l)