{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Marlowe validators.
--
-- Module      :  $Headers
-- License     :  Apache 2.0
--
-- Stability   :  Experimental
-- Portability :  Portable
module Language.Marlowe.Scripts.Types (
  -- * Types
  MarloweInput,
  MarloweTxInput (..),

  -- * Utilities
  marloweTxInputsFromInputs,
) where

import GHC.Generics (Generic)
import Language.Marlowe.Core.V1.Semantics.Types as Semantics
import Language.Marlowe.Pretty (Pretty (..))
import PlutusTx (makeIsDataIndexed, makeLift)
import PlutusTx.Prelude as PlutusTxPrelude hiding (traceError, traceIfFalse)
import Prelude qualified as Haskell

-- | Input to a Marlowe transaction.
type MarloweInput = [MarloweTxInput]

-- | A single input applied in the Marlowe semantics validator.
data MarloweTxInput
  = Input InputContent
  | MerkleizedTxInput InputContent BuiltinByteString
  deriving stock (Haskell.Show, Haskell.Eq, Generic)
  deriving anyclass (Pretty)

-- | Convert semantics input to transaction input.
marloweTxInputFromInput :: Input -> MarloweTxInput
marloweTxInputFromInput (NormalInput i) = Input i
marloweTxInputFromInput (MerkleizedInput i h _) = MerkleizedTxInput i h

-- | Convert semantics inputs to transaction inputs.
marloweTxInputsFromInputs :: [Input] -> [MarloweTxInput]
marloweTxInputsFromInputs = fmap marloweTxInputFromInput

-- Lifting data types to Plutus Core
makeLift ''MarloweTxInput
makeIsDataIndexed ''MarloweTxInput [('Input, 0), ('MerkleizedTxInput, 1)]
