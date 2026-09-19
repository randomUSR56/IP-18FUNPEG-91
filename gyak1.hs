-- jegyzet: negálás /= (nem úgy mint máshol: !=)
-- http://lambda.inf.elte.hu/Boolean.xml
-- http://lambda.inf.elte.hu/Chars.xml
-- Házi: tms.inf.elte.hu
module Gyak1 where

one :: Int
one = 1

doubleone :: Double
doubleone = 1

inc :: Int -> Int -- típusdeklaráció
inc i = i + 1 -- függvénydefiníció

multAdd :: Int -> Int -> Int
multAdd n m = n * m + n + m

not2 :: Bool -> Bool
-- not2 True = False
-- not2 False = True
not2 True = False
-- not2 x = True
not2 _ = True

-- ÉS művelet
myAnd0 :: Bool -> Bool -> Bool
myAnd0 True   True  = True
myAnd0 True   False = False
myAnd0 False  True  = False
myAnd0 False  False = False

-- prefix (|:|) VAGY művelet
-- (|:|) :: Bool -> Bool -> Bool
-- (|:|) False False = False
-- (|:|) _ _ = True

-- (|:|) True True = True
-- (|:|) True False = True
-- (|:|) False True = True

-- infix |:| VAGY művelet
(|:|) :: Bool -> Bool -> Bool
False |:| False = False
_ |:| _ = True

-- KIZÁRÓ VAGY (eXclusive OR)
xor :: Bool -> Bool -> Bool
True `xor` True = False
False `xor` False = False
_ `xor` _ = True

isA :: Char -> Bool
isA 'a' = True
isA 'A' = True
isA _ = False

isOne :: Int -> Bool
isOne 1 = True
isOne _ = False