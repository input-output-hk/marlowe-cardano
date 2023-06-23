module Marlowe.Contracts.Forward (
  forward,
) where

import Language.Marlowe.Extended.V1
import Marlowe.Contracts.Common
import Marlowe.Contracts.Swap

-- A forward contract is contract between to parties A and B to buy or sell an
-- underlying asset at a specified price on a future date
forward
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
  -- ^ Value B:ta
  -> Timeout
  -- ^ Deposit timeout B
  -> Timeout
  -- ^ Delivery date
  -> Contract
  -- ^ Forward contract
forward partyA tokenA valueA timeoutA partyB tokenB valueB timeoutB deliveryDate =
  waitUntil deliveryDate $
    swap
      partyA
      tokenA
      valueA
      timeoutA
      partyB
      tokenB
      valueB
      timeoutB
      Close
