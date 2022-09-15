{-# LANGUAGE OverloadedStrings #-}
module Marlowe.Contracts.UTC.Forward
  ( forward
  ) where

import Data.Time.Clock (UTCTime)
import Language.Marlowe.Extended.V1
import qualified Marlowe.Contracts.Forward as C
import Marlowe.Contracts.UTC.Common

-- A forward contract is contract between to parties A and B to buy or sell an
-- underlying asset at a specified price on a future date
forward ::
     Party    -- ^ Party A
  -> Token    -- ^ Token A
  -> Value    -- ^ Value A
  -> UTCTime  -- ^ Deposit timeout A
  -> Party    -- ^ Party B
  -> Token    -- ^ Token B
  -> Value    -- ^ Value B:ta
  -> UTCTime  -- ^ Deposit timeout B
  -> UTCTime  -- ^ Delivery date
  -> Contract -- ^ Forward contract
forward partyA tokenA valueA timeoutA partyB tokenB valueB timeoutB deliveryDate =
  C.forward partyA tokenA valueA (toTimeout timeoutA) partyB tokenB valueB (toTimeout timeoutB) (toTimeout deliveryDate)
