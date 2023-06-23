module Marlowe.Contracts.Swap where

import Language.Marlowe.Extended.V1
import Marlowe.Contracts.Common

-- | Swap tokens between two parties
swap
  :: Party
  -- ^ Party A
  -> Token
  -- ^ Token A
  -> Value
  -- ^ Value A
  -> Timeout
  -- ^ Deposit timeout A
  -> Party
  -- ^ Party B
  -> Token
  -- ^ Token B
  -> Value
  -- ^ Value B
  -> Timeout
  -- ^ Deposit timeout B
  -> Contract
  -- ^ Continuation
  -> Contract
  -- ^ Swap Contract
swap partyA tokenA valueA timeoutA partyB tokenB valueB timeoutB continuation =
  deposit partyA partyA (tokenA, valueA) timeoutA Close $
    deposit partyB partyB (tokenB, valueB) timeoutB Close $
      pay partyA partyB (tokenA, valueA) $
        pay
          partyB
          partyA
          (tokenB, valueB)
          continuation
