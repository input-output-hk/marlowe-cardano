{-# LANGUAGE OverloadedStrings #-}
module Marlowe.Contracts.Swap where

import Language.Marlowe
import Marlowe.Contracts.Common

-- |Swap tokens between two parties A and B
swap ::
     Party             -- ^ Party A
  -> Token             -- ^ Token A
  -> Value Observation -- ^ Value A
  -> Party             -- ^ Party B
  -> Token             -- ^ Token B
  -> Value Observation -- ^ Value B
  -> Timeout           -- ^ Deposit timeout
  -> Contract          -- ^ Continuation
  -> Contract          -- ^ Swap Cotnract
swap partyA tokenA valueA partyB tokenB valueB timeout continuation =
    deposit partyA partyA (tokenA, valueA) timeout Close
  $ deposit partyB partyB (tokenB, valueB) timeout Close
  $ pay partyA partyB (tokenA, valueA)
  $ pay partyB partyA (tokenB, valueB)
    continuation
