module Homework2 where
import Distribution.Compat.CharParsing (CharParsing(char))

-- csak akkor True ha paratlan szamu True van
xor3 :: Bool -> Bool -> Bool -> Bool
xor3 False False False = False
xor3 False True True = False
xor3 True True False = False
xor3 True False True = False
xor3 _ _ _ = True

-- csak akkor True ha pontosan egy darab True van
exactlyOneTrue :: Bool -> Bool -> Bool -> Bool
exactlyOneTrue False False False = False
exactlyOneTrue True True _ = False
exactlyOneTrue True _ True = False
exactlyOneTrue _ True True = False
exactlyOneTrue _ _ _ = True

-- toroljuk a megadott karaktert, amennyiben a masodik parameter True
deleteIf :: Char -> Bool -> Char
deleteIf _ True = ' '
deleteIf c False = c

-- emlitesre melto dolgok amiket megtanultam ebbol:
-- amennyiben hasznalni akarod egy fuggveny bemeneti valtozojat, adj neki nevet
-- ezert nem mukodne a deleteIf _ False = _
-- ebbol kovetkezik a masik dolog: a '_' karakter eldobja az erteket, nem tortenik binding.
-- viszont ha a fentebbi fuggvenyekben (xor3 es exactlyOneTrue) '_' helyett
-- barmi egyebet hasznaltam volna akkor is mukodne a kod (mert sehol sincs hasznalva a torzsben)
-- de adna egy warningo, hogy vannak nem hasznalt valtozoim.
-- stilisztikailag valoban jobb ha a wildcard tenyleg wildcard '_'