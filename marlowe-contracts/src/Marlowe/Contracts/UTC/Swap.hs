module Marlowe.Contracts.UTC.Swap
  where

import Data.Time.Clock (UTCTime)
import Language.Marlowe.Extended.V1
import qualified Marlowe.Contracts.Swap as C
import Marlowe.Contracts.UTC.Common

swap ::
     Party    -- ^ Party A
  -> Token    -- ^ Token A
  -> Value    -- ^ Value A
  -> UTCTime  -- ^ Deposit timeout A
  -> Party    -- ^ Party B
  -> Token    -- ^ Token B
  -> Value    -- ^ Value B
  -> UTCTime  -- ^ Deposit timeout B
  -> Contract -- ^ Continuation
  -> Contract -- ^ Swap Contract
swap partyA tokenA valueA timeoutA partyB tokenB valueB timeoutB =
  C.swap partyA tokenA valueA (toTimeout timeoutA) partyB tokenB valueB (toTimeout timeoutB)
