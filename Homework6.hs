module Homework6 where
dupElems :: [a] -> [a]
dupElems [] = []
dupElems (x:xs) = x:x:dupElems xs

injectDoubleElem :: a -> [a] -> [a]
injectDoubleElem _ [] = []
injectDoubleElem _ (x:[]) = (x:[])
injectDoubleElem y (x:xs) = x:y:y:injectDoubleElem y xs

elemIds :: Eq a => a -> [a] -> [Int]
elemIds i xs = go 0 xs
  where
    go _ [] = []
    go n (y:ys)
        | i == y    = n : go (n + 1) ys
        | otherwise = go (n + 1) ys