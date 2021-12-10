module Contrib.Halogen where

import Heterogeneous.Folding (class FoldingWithIndex, class HFoldlWithIndex, hfoldlWithIndex)
import Prim.Row (class Cons, class Lacks) as Row
import Prim.RowList (class RowToList) as RL
import Record (insert, modify) as Record
import Type.Prelude (class IsSymbol, Proxy(..))

-- | This is not strictly halogen specific...
-- |
-- | Given a proxy to the record (like state record)
-- | we produce here a dispatcher (another record).
-- |
-- | This dispatcher when used with `Variant.match`
-- | accepts a `Variant` value and dispatches its action
-- | using it's label in a type safe manner over our state value.
data ActionDispatchStep
  = ActionDispatchStep

instance purtyProblem ::
  ( IsSymbol l
  , Row.Cons l value state_ state
  , Row.Lacks l disp
  , Row.Cons l ((value -> value) -> ({ | state } -> { | state })) disp disp'
  ) =>
  FoldingWithIndex ActionDispatchStep (Proxy l) { | disp } (Proxy value) { | disp' } where
  foldingWithIndex _ l acc _ = Record.insert l (Record.modify l) acc

mkActionDispatcher :: forall r rl rout. RL.RowToList r rl => HFoldlWithIndex ActionDispatchStep {} (Proxy rl) { | rout } => Proxy { | r } -> { | rout }
mkActionDispatcher _ = hfoldlWithIndex ActionDispatchStep {} (Proxy :: Proxy rl)

type Update a
  = a -> a
