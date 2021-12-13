module Contrib.Type.Proxy.Generic where

import Heterogeneous.Folding (class FoldingWithIndex, class HFoldlWithIndex, hfoldlWithIndex)
import Prim.Row (class Cons, class Lacks) as Row
import Prim.RowList (class RowToList) as RL
import Record (insert) as Record
import Type.Prelude (class IsSymbol, Proxy(..))

-- | For a given proxy with a record produce a record of proxies with appropriate labels.
-- | We can avoid all these anoying Proxy definitions...
data LabelStep
  = LabelStep

instance purtyProblem ::
  ( IsSymbol l
  , Row.Cons l (Proxy l) proxies proxies'
  , Row.Lacks l proxies
  ) =>
  FoldingWithIndex LabelStep (Proxy l) { | proxies } value { | proxies' } where
  foldingWithIndex _ l acc _ = Record.insert l (Proxy :: Proxy l) acc

-- | Pass a **value** which type carries a row (like `Record` or `Variant` or even `Proxy`)
-- | to get record value back with lables proxies inside. For example:
-- | ```
-- | fromValueLabels { a :: Int, b :: String } -> { a :: Proxy "a", b :: Proxy "b" }
-- | fromValueLabels (Proxy (a :: Int}) -> { a :: Proxy "a" }
-- | ```
fromRowLabels :: forall r rl rout t. RL.RowToList r rl => HFoldlWithIndex LabelStep {} (Proxy rl) { | rout } => t r -> { | rout }
fromRowLabels _ = hfoldlWithIndex LabelStep {} (Proxy :: Proxy rl)

-- | Pass a proxy to a type which carries a row.
-- | It is just a conveninent unwrapping which allows you to derive a label
-- | proxies from a **type** like `Record` or `Variant`
-- | so you don't have to provide an actual value. For example:
-- | ```
-- | fromTypeLabels (Proxy { a :: Int, b :: String }) = { a :: Proxy "a", b :: Proxy "b" }
-- | ```
-- | This function allows you to avoid defining row types separate from your `Record` or `Variant`.
fromTypeRowLabels :: forall proxy r rl rout t. RL.RowToList r rl => HFoldlWithIndex LabelStep {} (Proxy rl) { | rout } => proxy (t r) -> { | rout }
fromTypeRowLabels _ = hfoldlWithIndex LabelStep {} (Proxy :: Proxy rl)
