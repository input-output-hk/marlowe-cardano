module Contrib.Record.Generic where

import Contrib.Type.Data.Symbol.Transform (class ToUpperFirst) as Symbol.Transform
import Heterogeneous.Folding (class FoldingWithIndex, class HFoldlWithIndex, hfoldlWithIndex)
import Prim.Row (class Cons, class Lacks) as Row
import Prim.RowList (class RowToList) as RL
import Prim.Symbol (class Append) as Symbol
import Record (insert, modify, set) as Record
import Type.Prelude (class IsSymbol, Proxy(..))

-- | Given a proxy to the record (like "state record")
-- | we produce a dispatcher - an another record.
-- |
-- | This dispatcher when used with `Variant.match`
-- | accepts a `Variant` value (an "action") and dispatches it
-- | using its label in a type safe manner over our state value.
-- |
-- | We provide two types of dispatchers below for setters and
-- | updaters.
-- | We use `FoldingWithIndex` here and below and not
-- | `MappingWithIndex` because mapping requires
-- | a value to operate on and folding allows us to build up
-- | a value "out of thin air" i.e. based on `Proxy (RowList k)`.
data SettersDispatchStep
  = SettersDispatchStep

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

-- | If you are brave enough and don't want to use setters with a clear semantic
-- | you can reach for updaters. The final action type algebra would be just ambigious...
-- | Updater dispatcher allows you to modify state field value in arbitrary way by
-- | applying "Update" function from a given `Variant` value over a
-- | corresponding `Record` field.
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
