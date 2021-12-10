module Contrib.Record where

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

-- | Pass a proxy with record to get record value back with lables proxies
-- |
-- | labels (Proxy { a :: Int, b :: String }) = { a :: Proxy "a", b :: Proxy "b" }
mkRecordProxies :: forall r rl rout. RL.RowToList r rl => HFoldlWithIndex LabelStep {} (Proxy rl) { | rout } => Proxy { | r } -> { | rout }
mkRecordProxies _ = hfoldlWithIndex LabelStep {} (Proxy :: Proxy rl)

-- | Pass a proxy with row to get a record value back with labels proxies
mkRowProxies :: forall r rl rout. RL.RowToList r rl => HFoldlWithIndex LabelStep {} (Proxy rl) { | rout } => Proxy r -> { | rout }
mkRowProxies _ = hfoldlWithIndex LabelStep {} (Proxy :: Proxy rl)
