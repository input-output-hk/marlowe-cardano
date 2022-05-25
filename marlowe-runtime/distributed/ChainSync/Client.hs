{-# LANGUAGE BlockArguments            #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeApplications          #-}
module ChainSync.Client where

import Cardano.Api
import Cardano.Api.Shelley (Hash (HeaderHash))
import Control.Distributed.Process (Closure, Process, ProcessId, expect, getSelfPid, nsend, say, send)
import Control.Distributed.Process.Closure (mkClosure, remotable)
import Control.Distributed.Process.Internal.Types (Process (..))
import Data.Binary (Binary (..))
import Data.Maybe (fromMaybe)
import Data.Typeable (Typeable)
import Data.Word (Word32, Word64)
import GHC.Generics (Generic)
import Language.Marlowe.Runtime.Chain (marloweChainSyncClient, runMarloweChainSyncClient)
import Language.Marlowe.Runtime.Chain.Types (MarloweBlockHeaderHash (..), MarloweChainEvent (..),
                                             MarloweChainPoint (..), MarloweChainTip (..), MarloweSlotNo (..))

data ChainSyncClientConfig = ChainSyncClientConfig
  { epochSlots   :: Word64
  , networkMagic :: Maybe Word32
  , nodeSocket   :: FilePath
  }
  deriving (Generic, Typeable, Show, Eq)

instance Binary ChainSyncClientConfig

data ChainSyncMsg
  = ChainSyncStart MarloweChainPoint MarloweChainTip
  | ChainSyncEvent MarloweChainEvent
  | ChainSyncDone
  deriving (Generic, Typeable, Show, Eq)

instance Binary ChainSyncMsg

newtype GetBlocks = GetBlocks ProcessId deriving (Eq, Ord, Show, Generic, Typeable)

instance Binary GetBlocks

newtype GetIntersectionPoints = GetIntersectionPoints ProcessId deriving (Eq, Ord, Show, Generic, Typeable)

instance Binary GetIntersectionPoints

chainSyncClient :: (ChainSyncClientConfig, ProcessId) -> Process ()
chainSyncClient (ChainSyncClientConfig{..}, parent) = do
  self <- getSelfPid
  nsend "chain-sync.store" $ GetIntersectionPoints self
  intersectionPoints <- expect
  let
    connectionInfo = LocalNodeConnectInfo
      { localConsensusModeParams = CardanoModeParams $ EpochSlots epochSlots
      , localNodeNetworkId = maybe Mainnet (Testnet . NetworkMagic) networkMagic
      , localNodeSocketPath = nodeSocket
      }
    toChainPoint MarloweChainPointAtGenesis = ChainPointAtGenesis
    toChainPoint (MarloweChainPoint (MarloweSlotNo slot) (MarloweBlockHeaderHash hash)) =
      ChainPoint (SlotNo slot) (HeaderHash hash)
  either fail id =<< runMarloweChainSyncClient connectionInfo \genesisParams -> Process do
    unProcess do
      say "Starting chain sync client"
      say $ "Network: " <> show (localNodeNetworkId connectionInfo)
      say $ "Local node socket path: " <> show nodeSocket
    marloweChainSyncClient
      (toChainPoint <$> intersectionPoints)
      genesisParams
      (fmap (unProcess . send parent) . ChainSyncStart . fromMaybe MarloweChainPointAtGenesis)
      (pure False)
      (unProcess . send parent . ChainSyncEvent)
      (unProcess $ send parent ChainSyncDone)

remotable ['chainSyncClient]

process :: ChainSyncClientConfig -> ProcessId -> Closure (Process ())
process = curry $(mkClosure 'chainSyncClient)
