module Marlowe.Execution.Lenses
  ( _semanticState
  , _contract
  , _history
  , _previousTransactions
  , _mPendingTimeouts
  , _pendingTimeouts
  , _mNextTimeout
  , _balancesAtStart
  , _txInput
  , _balancesAtEnd
  , _resultingPayments
  , _continuationState
  , _continuationContract
  ) where

import Prologue

import Data.DateTime.Instant (Instant)
import Data.Lens (Lens', Traversal', _Just, traversed)
import Data.Lens.Record (prop)
import Data.List (List)
import Language.Marlowe.Core.V1.Semantics.Types
  ( Accounts
  , Contract
  , Payment
  , TransactionInput
  )
import Language.Marlowe.Core.V1.Semantics.Types (State) as Semantic
import Marlowe.Execution.Types
  ( ContractAndState
  , PastState
  , PendingTimeouts
  , State
  , TimeoutInfo
  )
import Type.Proxy (Proxy(..))

_semanticState :: Lens' State Semantic.State
_semanticState = prop (Proxy :: _ "semanticState")

_contract :: Lens' State Contract
_contract = prop (Proxy :: _ "contract")

_history :: Lens' State (Array PastState)
_history = prop (Proxy :: _ "history")

_previousTransactions :: Traversal' State TransactionInput
_previousTransactions = _history <<< traversed <<< _txInput

_mPendingTimeouts :: Lens' State (Maybe PendingTimeouts)
_mPendingTimeouts = prop (Proxy :: _ "mPendingTimeouts")

_pendingTimeouts :: Traversal' State (Array TimeoutInfo)
_pendingTimeouts = _mPendingTimeouts <<< _Just <<< _timeouts

_mNextTimeout :: Lens' State (Maybe Instant)
_mNextTimeout = prop (Proxy :: _ "mNextTimeout")

----------
_balancesAtStart :: Lens' PastState Accounts
_balancesAtStart = prop (Proxy :: _ "balancesAtStart")

_txInput :: Lens' PastState TransactionInput
_txInput = prop (Proxy :: _ "txInput")

_balancesAtEnd :: Lens' PastState Accounts
_balancesAtEnd = prop (Proxy :: _ "balancesAtEnd")

_resultingPayments :: Lens' PastState (List Payment)
_resultingPayments = prop (Proxy :: _ "resultingPayments")

----------
_continuation :: Lens' PendingTimeouts ContractAndState
_continuation = prop (Proxy :: _ "continuation")

_continuationState :: Lens' PendingTimeouts Semantic.State
_continuationState = _continuation <<< prop (Proxy :: _ "state")

_continuationContract :: Lens' PendingTimeouts Contract
_continuationContract = _continuation <<< prop (Proxy :: _ "contract")

_timeouts :: Lens' PendingTimeouts (Array TimeoutInfo)
_timeouts = prop (Proxy :: _ "timeouts")
