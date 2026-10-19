module FPEA_06 where

-- null
reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

reverseT :: [a] -> [a]
reverseT ls = revTH ls []
    where
        revTH []        acc = acc
        revTH (x:xs) acc = revTH xs (x:acc)

lengthT :: [a] -> Int
lengthT ls = lenH ls 0
    where
        lenH [] acc = acc
        lenH (_:xs) acc = lenH xs (acc+1)

and' :: [Bool] -> Bool
and' [] = True
and' (x:xs) = x && and' xs

andT :: [Bool] -> Bool
andT ls = andH ls True
    where
        andH [] b = b
        andH (x:xs) b = andH xs (b && x)

everySnd :: [a] -> [a]
everySnd (_:y:xs) = y : everySnd xs
everySnd _ = []

everyNth :: [a] -> Int -> [a]
everyNth ls n | n > 0 = everyH ls 1
    where
        everyH [] _ = []
        everyH (x:xs) i
            | i `mod` n == 0 = x : everyH xs (i+1)
            | otherwise = everyH xs (i+1)

eqList :: Eq a => [a] -> [a] -> Bool
eqList [] [] = True
eqList (x:xs) (y:ys) = x == y && eqList xs ys
eqList _ _ = False

inits' :: [a] -> [[a]]
inits' [] = [] : [] -- [[]]
-- inits' (x:xs) = [] :