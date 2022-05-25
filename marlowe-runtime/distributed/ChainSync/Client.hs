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
import Control.Distributed.Process (Closure, Process, ProcessId, say, send)
import Control.Distributed.Process.Closure (mkClosure, remotable)
import Control.Distributed.Process.Internal.Types (Process (Process, unProcess))
import Data.Binary (Binary (..))
import Data.Maybe (fromMaybe)
import Data.Typeable (Typeable)
import Data.Word (Word32, Word64)
import GHC.Generics (Generic)
import Language.Marlowe.Runtime.Chain (marloweChainSyncClient, runMarloweChainSyncClient)
import Language.Marlowe.Runtime.Chain.Types (MarloweChainEvent (..), MarloweChainPoint (..), MarloweChainTip (..))

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

chainSyncClient :: (ChainSyncClientConfig, ProcessId) -> Process ()
chainSyncClient (ChainSyncClientConfig{..}, parent) = do
  let
    connectionInfo = LocalNodeConnectInfo
      { localConsensusModeParams = CardanoModeParams $ EpochSlots epochSlots
      , localNodeNetworkId = maybe Mainnet (Testnet . NetworkMagic) networkMagic
      , localNodeSocketPath = nodeSocket
      }
  either fail id =<< runMarloweChainSyncClient connectionInfo \genesisParams -> Process do
    unProcess do
      say "Starting chain sync client"
      say $ "Network: " <> show (localNodeNetworkId connectionInfo)
      say $ "Local node socket path: " <> show nodeSocket
    marloweChainSyncClient
      []
      genesisParams
      (fmap (unProcess . send parent) . ChainSyncStart . fromMaybe MarloweChainPointAtGenesis)
      (pure False)
      (unProcess . send parent . ChainSyncEvent)
      (unProcess $ send parent ChainSyncDone)

remotable ['chainSyncClient]

process :: ChainSyncClientConfig -> ProcessId -> Closure (Process ())
process = curry $(mkClosure 'chainSyncClient)
