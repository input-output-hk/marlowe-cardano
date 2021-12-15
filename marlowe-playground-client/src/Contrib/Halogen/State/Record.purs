module Contrib.Halogen.State.Record where

import Contrib.Type.Data.Symbol.Transform (class ToUpperFirst) as Symbol.Transform
import Heterogeneous.Folding (class FoldingWithIndex, class HFoldlWithIndex, hfoldlWithIndex)
import Prim.Row (class Cons, class Lacks) as Row
import Prim.RowList (class RowToList) as RL
import Prim.Symbol (class Append) as Symbol
import Record (insert, modify, set) as Record
import Type.Prelude (class IsSymbol, Proxy(..))

-- | This is not strictly halogen specific folding...
-- |
-- | Given a proxy to the record (like "state record")
-- | we produce a dispatcher (another record).
-- |
-- | This dispatcher when used with `Variant.match`
-- | accepts a `Variant` value (an "action") and dispatches it
-- | using its label in a type safe manner over our state value.
data SettersDispatchStep
  = SettersDispatchStep

-- | We use `FoldingWithIndex` here and not
-- | `MappingWithIndex` because mapping requires
-- | a value to operate on and folding allows us to build up
-- | a value just from the `Proxy (RowList k)`.
instance purtyProblem0 ::
  ( IsSymbol l
  , Symbol.Transform.ToUpperFirst l l'
  , Symbol.Append "set" l' l''
  , IsSymbol l''
  , Row.Cons l value state_ state
  , Row.Lacks l'' disp
  , Row.Cons l'' (value -> ({ | state } -> { | state })) disp disp'
  ) =>
  FoldingWithIndex SettersDispatchStep (Proxy l) { | disp } (Proxy value) { | disp' } where
  foldingWithIndex _ l acc _ = Record.insert (Proxy :: Proxy l'') (Record.set l) acc

mkSettersDispatcher ::
  forall r rl rout.
  RL.RowToList r rl =>
  HFoldlWithIndex SettersDispatchStep {} (Proxy rl) { | rout } =>
  Proxy { | r } ->
  { | rout }
mkSettersDispatcher _ = hfoldlWithIndex SettersDispatchStep {} (Proxy :: Proxy rl)

-- | If are brave and don't want to use setters with quite clear semantics
-- | you can reach for updaters. They allow you to modify state field value
-- | in arbitrary way. The final action type algebra would be just
-- | ambigious...
data UpdatersDispatchStep
  = UpdatersDispatchStep

type Update a
  = a -> a

instance purtyProblem1 ::
  ( IsSymbol l
  , Symbol.Transform.ToUpperFirst l l'
  , Symbol.Append "update" l' l''
  , IsSymbol l''
  , Row.Cons l value state_ state
  , Row.Lacks l'' disp
  , Row.Cons l'' (Update value -> Update { | state }) disp disp'
  ) =>
  FoldingWithIndex UpdatersDispatchStep (Proxy l) { | disp } (Proxy value) { | disp' } where
  foldingWithIndex _ l acc _ = Record.insert (Proxy :: Proxy l'') (Record.modify l) acc

mkUpdatersDispatcher ::
  forall r rl rout.
  RL.RowToList r rl =>
  HFoldlWithIndex UpdatersDispatchStep {} (Proxy rl) { | rout } =>
  Proxy { | r } ->
  { | rout }
mkUpdatersDispatcher _ = hfoldlWithIndex UpdatersDispatchStep {} (Proxy :: Proxy rl)
