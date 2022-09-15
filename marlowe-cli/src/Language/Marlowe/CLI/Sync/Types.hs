-----------------------------------------------------------------------------
--
-- Module      :  $Headers
-- License     :  Apache 2.0
--
-- Stability   :  Experimental
-- Portability :  Portable
--
-- | Chain-sync client types for Cardano node.
--
-----------------------------------------------------------------------------


{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Language.Marlowe.CLI.Sync.Types
  ( -- * Types
    MarloweAddress(..)
  , MarloweAddresses
  , MarloweEvent(..)
  , MarloweIn(..)
  , MarloweOut(..)
  , SavedPoint(..)
  ) where


import Cardano.Api (BlockHeader(..), ChainPoint(..), ChainTip, Hash, ScriptHash, SlotNo(..), TxId, TxIn, Value)
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Language.Marlowe.CLI.Orphans ()
import Language.Marlowe.Core.V1.Semantics (MarloweData(..), MarloweParams(..))
import Language.Marlowe.Core.V1.Semantics.Types (Input, TimeInterval)
import Plutus.V1.Ledger.Api (Address, TokenName)

import qualified Data.Aeson as A (Value)
import qualified Data.Map.Strict as M (Map)


-- | A point on the chain.
data SavedPoint a =
  SavedPoint
  {
    slotNo    :: SlotNo            -- ^ The slot number.
  , blockHash :: Hash BlockHeader  -- ^ The block hash.
  , memory    :: a                 -- ^ The additional information to be recorded.
  }
    deriving (Eq, FromJSON, Generic, Ord, Show, ToJSON)


-- | Map from Marlowe addresses to parameters.
type MarloweAddresses = M.Map MarloweAddress MarloweParams


-- | A Marlowe address.
data MarloweAddress =
    ApplicationCredential ScriptHash  -- ^ The Marlowe application's address.
  | PayoutCredential ScriptHash       -- ^ The Marlowe payout's address.
    deriving (Eq, Generic, FromJSON, Ord, Show, ToJSON)


-- | Transaction input relevant to Marlowe.
data MarloweIn =
    -- | Non-Marlowe transaction input.
    PlainIn
    {
      miTxIn :: TxIn  -- ^ The transaction input.
    }
    -- | Input to the Marlowe application script.
  | ApplicationIn
    {
      miTxIn   :: TxIn     -- ^ The transaction input.
    , miInputs :: [Input]  -- ^ The Marlowe inputs.
    }
  | PayoutIn
    {
      miTxIn   :: TxIn       -- ^ The transaction input.
    , miPayout :: TokenName  -- ^ The role token for the payout.
    }
    deriving (Eq, Generic, FromJSON, Show, ToJSON)


-- | Transaction output relevant to Marlowe.
data MarloweOut =
    -- | Non-Marlowe transaction output.
    PlainOut
    {
      moTxIn    :: TxIn     -- ^ The transaction input being produced.
    , moAddress :: Address  -- ^ The address receiving the output.
    , moValue   :: Value    -- ^ The value output.
    }
    -- | Output to the Marlowe application script.
  | ApplicationOut
    {
      moTxIn    :: TxIn         -- ^ The transaction input being produced.
    , moAddress :: Address      -- ^ The address receiving the output.
    , moValue   :: Value        -- ^ The value output.
    , moOutput  :: MarloweData  -- ^ The Marlowe data in the output.
    }
    -- | Output to the Marlowe payout script.
  | PayoutOut
    {
      moTxIn    :: TxIn       -- ^ The transaction input being produced.
    , moAddress :: Address    -- ^ The address receiving the output.
    , moValue   :: Value      -- ^ The value output.
    , moPayout  :: TokenName  -- ^ The role token for the payout.
    }
    deriving (Eq, Generic, FromJSON, Show, ToJSON)


-- | A Marlowe event on the blockchain.
data MarloweEvent =
    -- | Marlowe parameters associated with a minting transaction.
    Parameters
    {
      meBlock              :: BlockHeader     -- ^ The block header for the Marlowe transaction.
    , meTxId               :: TxId            -- ^ The ID of the Marlowe transaction.
    , meApplicationAddress :: MarloweAddress  -- ^ The address of the Marlowe application validator.
    , mePayoutAddress      :: MarloweAddress  -- ^ The address of the Marlowe payout validator.
    , meParams             :: MarloweParams   -- ^ The Marlowe validator parameters.
    , meMetadata           :: Maybe A.Value   -- ^ The transaction metadata, if any.
    }
    -- | A Marlowe transaction.
  | Transaction
    {
      meBlock    :: BlockHeader         -- ^ The block header for the Marlowe transaction.
    , meTxId     :: TxId                -- ^ The ID of the Marlowe transaction.
    , meIns      :: [MarloweIn]         -- ^ The transaction inputs.
    , meOuts     :: [MarloweOut]        -- ^ The transaction outputs, except for the output to the Marlowe validators.
    , meInterval :: Maybe TimeInterval  -- ^ The validity range of the transaction.
    , meMetadata :: Maybe A.Value       -- ^ The transaction metadata, if any.
    }
    -- | An anomalous Marlowe transaction.
  | Anomaly
    {
      meBlock   :: BlockHeader  -- ^ The block header for the Marlowe transaction.
    , meTxId    :: TxId         -- ^ The ID of the Marlowe transaction.
    , meAnomaly :: String       -- ^ A description of the anomaly.
    }
  | -- | A Block.
    NewBlock
    {
      meBlock   :: BlockHeader  -- ^ The block header for the Marlowe transaction.
    }
    -- | A rollback.
  | Rollback
    {
      rollbackPoint :: ChainPoint  -- ^ The point to which the rollback occurred.
    , rollbackTip   :: ChainTip    -- ^ The tip when the rollback occurred.
    }
  deriving (Generic, Show, ToJSON)
