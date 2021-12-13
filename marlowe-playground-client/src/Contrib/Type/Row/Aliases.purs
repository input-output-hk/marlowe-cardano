module Contrib.Type.Row.Aliases where

import Data.Symbol (class IsSymbol)
import Prim.Row (class Cons) as Row

class Cons' :: forall k. Symbol -> k -> Row k -> Row k -> Constraint
class (IsSymbol l, Row.Cons l a r r') <= Cons' l a r r'

instance purtyProblem :: (IsSymbol l, Row.Cons l a r r') => Cons' l a r r'
