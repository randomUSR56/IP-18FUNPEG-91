module Homework7 where

mirrorP :: (Integer, Integer) -> (Integer, Integer) -> (Integer, Integer)
mirrorP (x1, y1) (x2, y2) = (2 * x1 - x2, 2 * y1 - y2)

range :: [Integer] -> [Integer]
range []          = []
range [a]         = [a ..]
range [a, b]      = [a, b ..]
range (a : b : c : _) = [a, b .. c]

myConcat :: [[a]] -> [a]
myConcat []       = []
myConcat (x : xs) = x ++ myConcat xs

runs :: Int -> [a] -> [[a]]
runs _ [] = []
runs n xs = take n xs : runs n (drop n xs)

nonEmpties :: [[a]] -> [[a]]
nonEmpties []         = []
nonEmpties ([] : xss) = nonEmpties xss
nonEmpties (xs : xss) = xs : nonEmpties xss