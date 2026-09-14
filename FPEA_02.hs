module FPEA_02 where

-- inc :: Int -> Int
-- inc i = i + 1


incInt :: Int -> Int
incInt i = i + 1

incInteger :: Integer -> Integer
incInteger i = i + 1

-- Integral = leszűkítés egész típusokra (egészek csoportja) (vagy Int-et vagy Integer-t vár)
-- Más csoportoknál is így van
-- Mi az a Num?
-- segít az :i Num
-- Túlterhelhető típusosztály
-- Magában foglalja az előbb említetteket is (pl.: Fraction, Integral etc..)
-- Azt is megadja milyen aritmetikai műveletek végezhetőek a típusokon
-- Az '/' művelete csak a Double és Float-oknál lehetséges (Fraction)
-- Hogyan osztunk egészeket? 
-- div, mod (osztás és maradékképzés)
inc :: Integral a => a -> a
inc i = i + 1

doubleAdd :: Int -> Int -> Int
doubleAdd n m = 2 * n + m

-- Totális vs Parciális definiáltság (függvények esetében)
-- (pl.: mintaillesztésnél minden lehetséges kimenetet lefedtél-e vagy nem)
isZero :: Int -> Bool
-- isZero i = i == 0
isZero 0 = True
isZero _ = False

-- Amennyiben nem adjuk meg a típust kézzel
-- ifZero :: (Eq a1, Num a1, Num a2) => a1 -> a2 -- erről később bővebben..
-- Ha a bementi típustól függ a kimeneti az külön meg lesz jelölve (egyébkent független pl.: a1 -> a2)
ifZero :: Int -> Int
-- Konkrét értékkel vizsgált eset
ifZero 0 = 10
-- Általános eset (általában joker karakterrel jelölve '_')
ifZero _ = -10
-- Int és Integer NEM ugyanaz a háttérben
-- :: megkötés NEM konverzió
-- Van syntactic sugar, 2 / 4 esetében A és B automatikusan Double anélkül, hogy explicit megkötnénk, hogy :: Double
-- (mert a '2' polymorph számliterál)
-- (2 = 2.0 kontextustól függően)
-- (azonban fordítva nem igaz. minél jobban leírsz valamit annál szigorúbb lesz 2.0 NEM 2 hanem fixen NEM Int)

divides :: Int -> Int -> Bool
-- Kompozíció (egyik függvén megkapja a másik értékét f(x), g(x) => f(g(x)))
divides n m = isZero (mod m n)
-- divides n m =  mod m n == 0

divisors :: Int -> [Int]
-- Lineáris keresés: keressük Int-nek Int osztóit
divisors n = [ i | i <- [1..n], i `divides` n]

isPrime :: Int -> Bool
--isPrime n = length (divisors n) == 2
-- Miért nem jó ez?
-- A lusta kiértékelés miatt
-- Más szemlélettel kell megterveznünk az algoritmusunk
isPrime n = divisors n == [1, n]

