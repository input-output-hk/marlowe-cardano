module Data.Variant.Generic where

import Prelude

import Data.Variant (Variant)
import Data.Variant (inj) as Variant
import Heterogeneous.Folding
  ( class FoldingWithIndex
  , class HFoldlWithIndex
  , hfoldlWithIndex
  )
import Prim.Row (class Cons, class Lacks) as Row
import Prim.RowList (class RowToList)
import Record (insert) as Record
import Type.Prelude (class IsSymbol, Proxy(..))

data ConstructorStep :: Row Type -> Type
data ConstructorStep variant = ConstructorStep

instance
  ( IsSymbol l
  , Row.Cons l Unit variant_ variant
  , Row.Lacks l constructors
  , Row.Cons l (Variant variant) constructors constructors'
  ) =>
  FoldingWithIndex (ConstructorStep variant)
    (Proxy l)
    { | constructors }
    (Proxy Unit)
    { | constructors' } where
  foldingWithIndex _ l acc _ = Record.insert l (Variant.inj l unit) acc
else instance
  ( IsSymbol l
  , Row.Cons l value variant_ variant
  , Row.Lacks l constructors
  , Row.Cons l (value -> (Variant variant)) constructors constructors'
  ) =>
  FoldingWithIndex (ConstructorStep variant)
    (Proxy l)
    { | constructors }
    (Proxy value)
    { | constructors' } where
  foldingWithIndex _ l acc _ = Record.insert l (Variant.inj l) acc

-- | Creates a record with helper functions which construct `Variant`
-- | so we can avoid using `Proxy`.
-- | ```
-- | type Example = Variant (p1 :: Unit, p2 :: Int, p3 :: String)
-- |
-- | mkExample :: { p1 :: Example, p2 :: Int -> Example, p3 :: String -> Example }
-- | mkExample = mkConstructors (Proxy :: Proxy Example)
-- | ```
mkConstructors
  :: forall constructors v vl
   . RowToList v vl
  -- => Eval (Constructors (Variant v) (Variant v)) constructors
  => HFoldlWithIndex (ConstructorStep v) {} (Proxy vl) { | constructors }
  => Proxy (Variant v)
  -> { | constructors }
mkConstructors _ = hfoldlWithIndex (ConstructorStep :: ConstructorStep v) {}
  (Proxy :: Proxy vl)

-- | If we want to remove the details about the constructors record type
-- | from the signature we can use this "alias" + `mkConstructors'`:
-- | ```
-- | mkExample :: forall cs. Constructors Example cs -> cs
-- | mkExample = mkConstructors' (Proxy :: Proxy Example)
-- | ```
class Constructors :: Type -> Type -> Constraint
class Constructors v c | v -> c where
  mkConstructors' :: Proxy v -> c

instance
  ( RowToList v vl
  , HFoldlWithIndex (ConstructorStep v) {} (Proxy vl) { | constructors }
  ) =>
  Constructors (Variant v) { | constructors } where
  mkConstructors' = mkConstructors

