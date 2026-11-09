module Gyak8 where
import Distribution.Simple (KnownExtension(FlexibleInstances))

isZero :: (Num a, Eq a) => a -> Bool
isZero x = x == 0
-- isZero 0 = True
-- isZero _ = False

data Answer = No | Yes | Maybe deriving Show
toString :: Answer -> String
toString No = "No"
toString Maybe = "Maybe"
toString Yes = "Yes"

data Pair = P Int Int
addElems (P x y) = x + y

data Day = Mon | Tue | Wed | Thu | Fri | Sat | Sun deriving Show
isWeekend :: Day -> Bool
isWeekend Sat = True
isWeekend Sun = True
isWeekend _ = False

data USTime = AM Int Int | PM Int Int deriving (Show, Eq)
showUSTime :: USTime -> String
showUSTime (AM h m) = show h ++ "." ++ show m ++ " am"
showUSTime (PM h m) = show h ++ "." ++ show m ++ " pm"