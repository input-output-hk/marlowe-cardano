{-# LANGUAGE BlockArguments            #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DeriveAnyClass            #-}
{-# LANGUAGE DerivingStrategies        #-}
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
import ChainSync.Database (ChainSyncQuery, getIntersectionPoints)
import Control.Distributed.Process (Closure, Process, SendPort, say, sendChan)
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

data ChainSyncClientDependencies = ChainSyncClientDependencies
  { config    :: ChainSyncClientConfig
  , msgChan   :: SendPort ChainSyncMsg
  , sendQuery :: SendPort ChainSyncQuery
  }
  deriving (Generic, Typeable, Show, Eq)

instance Binary ChainSyncClientDependencies

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

data SendPortWithRollback a = SendPortWithRollback
  { onResponse :: SendPort a
  , onRollback :: SendPort MarloweChainPoint
  }
  deriving (Generic, Typeable, Show, Eq, Ord)
  deriving anyclass Binary

chainSyncClient :: ChainSyncClientDependencies -> Process ()
chainSyncClient ChainSyncClientDependencies{..} = do
  let ChainSyncClientConfig{..} = config
  intersectionPoints <- getIntersectionPoints sendQuery
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
      (fmap (unProcess . sendChan msgChan) . ChainSyncStart . fromMaybe MarloweChainPointAtGenesis)
      (pure False)
      (unProcess . sendChan msgChan . ChainSyncEvent)
      (unProcess $ sendChan msgChan ChainSyncDone)

remotable ['chainSyncClient]

process :: ChainSyncClientDependencies -> Closure (Process ())
process = $(mkClosure 'chainSyncClient)
