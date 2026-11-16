module Gyak9 where
import Data.Char (isDigit, digitToInt)

maybeAdd :: Num a => Maybe a -> Maybe a -> Maybe a
maybeAdd (Just x) (Just y) = Just (x + y)
maybeAdd (Just x) Nothing = Just x
maybeAdd Nothing (Just y) = Just y
maybeAdd _ _ = Nothing

maybeSum :: Num a => [Maybe a] -> a
maybeSum [] = 0
maybeSum (Just x:xs) = x + (maybeSum xs)
maybeSum (Nothing:xs) = maybeSum xs

elemIndex :: Eq a => a -> [a] -> Maybe Int
elemIndex e xs = helpInd e xs 0 where
    helpInd _ [] _ = Nothing
    helpInd e (x:xs) i
        | e == x = Just i
        | otherwise = helpInd e xs (i+1)

-- Ez egy jo pelda!! Vizsgan is elofordulhat
readInt :: [Char] -> Maybe Int
readInt [] = Nothing                -- an empty string is not a number
readInt s  = accumulator s 0
  where
    accumulator :: [Char] -> Int -> Maybe Int
    accumulator [] acc = Just acc   -- string finished: the accumulator is the result
    accumulator (c:cs) acc
      | isDigit c = accumulator cs (acc * 10 + digitToInt c)
      | otherwise = Nothing         -- a non-digit means the input is invalid

data List a = Nil | Cons a (List a) deriving Show

list2ConsList :: [a] -> List a
list2ConsList [] = Nil
list2ConsList (x:xs) = x `Cons` list2ConsList xs