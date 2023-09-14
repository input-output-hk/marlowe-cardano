module Contrib.Cardano.UTxO where

import Cardano.Api qualified as C
import Data.Map.Strict qualified as Map

toList :: forall era. C.UTxO era -> [(C.TxIn, C.TxOut C.CtxUTxO era)]
toList = Map.toList . C.unUTxO

fromList :: forall era. [(C.TxIn, C.TxOut C.CtxUTxO era)] -> C.UTxO era
fromList = C.UTxO . Map.fromList
