module Gyak3 where
import Data.Char
multiply :: Integer -> Integer -> Integer
multiply _ 0 = 0
multiply 0 _ = 0
multiply x y
    | y>0 = (x + multiply x (y-1))
    | y<0 = (-x + multiply x (y+1))
    -- | eset eseten legyen a megoldas a kifejezes ertekevel egyenlo
    -- | (case) = (expr.)

powerN :: Integer -> Integer -> Integer
powerN 0 _ = 0
powerN _ 0 = 1
powerN x y = (x * powerN x (y-1))

minInt :: Int -> Int -> Int
minInt x y
    | x <= y  =  x
    | otherwise  =  y