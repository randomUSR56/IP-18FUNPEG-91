module Homework9 where

data USTime = AM Int Int | PM Int Int deriving (Show, Eq)
showUSTime :: USTime -> String
showUSTime (AM h m) = show h ++ "." ++ show m ++ " am"
showUSTime (PM h m) = show h ++ "." ++ show m ++ " pm"