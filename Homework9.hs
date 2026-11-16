module Homework9 where

data USTime = AM Int Int | PM Int Int deriving (Show, Eq)
showUSTime :: USTime -> String
showUSTime (AM h m) = show h ++ "." ++ show m ++ " am"
showUSTime (PM h m) = show h ++ "." ++ show m ++ " pm"

data CardinalPoint = North | East | South | West deriving (Show, Eq, Enum)
numOfDirection :: CardinalPoint -> [CardinalPoint] -> Integer
numOfDirection _ [] = 0
numOfDirection direction (x:xs)
    | direction == x    = 1 + numOfDirection direction xs
    | otherwise         = numOfDirection direction xs

turnLeft :: CardinalPoint -> CardinalPoint
turnLeft North = West
turnLeft East  = North
turnLeft South = East
turnLeft West  = South

turnLeft' :: CardinalPoint -> CardinalPoint
turnLeft' direction = toEnum ((fromEnum direction - 1) `mod` 4)

data Time = T Int Int deriving (Eq, Show)
time2USTime :: Time -> USTime
time2USTime (T h m)
    | h == 0    = AM 12 m
    | h < 12    = AM h m
    | h == 12   = PM 12 m
    | otherwise = PM (h - 12) m

uSTime2Time :: USTime -> Time
uSTime2Time (AM h m)
    | h == 12   = T 0 m
    | otherwise = T h m
uSTime2Time (PM h m)
    | h == 12   = T 12 m
    | otherwise = T (h + 12) m