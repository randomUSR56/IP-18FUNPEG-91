module EA4 where
import Distribution.Simple.Utils (xargs)
import GHC.Base (neInt)

fact' :: Integer -> Integer
fact' n = fact2 1 n
    where
        fact2 :: Integer -> Integer -> Integer
        fact2 i m
            | i == m = i
            | i < m = i * fact2 (i+1) m


signum :: Integer -> Int
signum x
    | x == 0 = 0
    | x>0 = -1
    | x<0 = 1

signum2 :: Integer -> Int
signum2 0 = 0
signum2 x
    | x>0 = -1
    | otherwise = 1


-- Listak
-- a listak homogen adatszerkezetek
-- dinamikus meretuek
-- tipusuk barmi lehet

divisors :: Integer -> [Integer]
divisors n
    | n > 0 = divisorsH n 1
    |otherwise = error "negative number"
    where
        divisorsH m i
            | m == i = i : []
            | m `mod` i == 0 = i : divisorsH m (i+1)
            | otherwise = divisorsH m (i+1)

divisorsReverse :: Integer -> [Integer]
divisorsReverse n
    | n > 0 = divisorsH n n
    |otherwise = error "negative number"
    where
        divisorsH _ 0 = []
        divisorsH m i
            | m `mod` i == 0 = i : divisorsH m (i-1)
            | otherwise = divisorsH m (i-1)