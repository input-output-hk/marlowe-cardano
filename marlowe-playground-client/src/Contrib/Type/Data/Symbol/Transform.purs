module Contrib.Type.Data.Symbol.Transform where

import Prim.Symbol (class Cons) as Symbol

class ToUpper (s :: Symbol) (s' :: Symbol) | s -> s'

instance purtyProblemA :: ToUpper "a" "A"

instance purtyProblemB :: ToUpper "b" "B"

instance purtyProblemC :: ToUpper "c" "C"

instance purtyProblemD :: ToUpper "d" "D"

instance purtyProblemE :: ToUpper "e" "E"

instance purtyProblemF :: ToUpper "f" "F"

instance purtyProblemG :: ToUpper "g" "G"

instance purtyProblemH :: ToUpper "h" "H"

instance purtyProblemI :: ToUpper "i" "I"

instance purtyProblemJ :: ToUpper "j" "J"

instance purtyProblemK :: ToUpper "k" "K"

instance purtyProblemL :: ToUpper "l" "L"

instance purtyProblemM :: ToUpper "m" "M"

instance purtyProblemN :: ToUpper "n" "N"

instance purtyProblemO :: ToUpper "o" "O"

instance purtyProblemP :: ToUpper "p" "P"

instance purtyProblemQ :: ToUpper "q" "Q"

instance purtyProblemR :: ToUpper "r" "R"

instance purtyProblemS :: ToUpper "s" "S"

instance purtyProblemT :: ToUpper "t" "T"

instance purtyProblemU :: ToUpper "u" "U"

instance purtyProblemV :: ToUpper "v" "V"

instance purtyProblemW :: ToUpper "w" "W"

instance purtyProblemX :: ToUpper "x" "X"

instance purtyProblemY :: ToUpper "y" "Y"

instance purtyProblemZ :: ToUpper "z" "Z"

class ToUpperFirst (s :: Symbol) (s' :: Symbol) | s -> s'

instance purtyProblem :: (Symbol.Cons h t s, ToUpper h h', Symbol.Cons h' t s') => ToUpperFirst s s'
