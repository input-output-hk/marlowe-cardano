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
import Control.Distributed.Process (Closure, Process, SendPort, matchChan, newChan, receiveChan, receiveWait, say,
                                    sendChan)
import Control.Distributed.Process.Closure (mkClosure, remotable)
import Control.Distributed.Process.Internal.Types (Process (..))
import Data.Binary (Binary (..))
import Data.Maybe (fromMaybe)
import Data.Typeable (Typeable)
import Data.Word (Word32, Word64)
import GHC.Generics (Generic)
import Language.Marlowe.Runtime.Chain (marloweChainSyncClient, runMarloweChainSyncClient)
import Language.Marlowe.Runtime.Chain.Types (MarloweBlockHeader (..), MarloweBlockHeaderHash (..), MarloweBlockNo (..),
                                             MarloweChainEvent (..), MarloweChainPoint (..), MarloweChainTip (..),
                                             MarloweSlotNo (..), MarloweTx (..), TxOutRef (..))

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

data TxWithBlockHeader = TxWithBlockHeader
  { blockHeader :: MarloweBlockHeader
  , tx          :: MarloweTx
  }
  deriving (Generic, Typeable, Show, Eq)
  deriving anyclass Binary

data SendPortWithRollback a = SendPortWithRollback
  { onResponse :: SendPort a
  , onRollback :: SendPort MarloweChainPoint
  }
  deriving (Generic, Typeable, Show, Eq, Ord)
  deriving anyclass Binary

data ChainSyncQuery
  = QueryTxThatConsumes TxOutRef (SendPortWithRollback TxWithBlockHeader)
  | QueryBlockNo MarloweBlockNo  (SendPortWithRollback MarloweBlockHeader)
  | QueryIntersectionPoints (SendPort [MarloweChainPoint])
  deriving (Generic, Typeable, Show, Eq)
  deriving anyclass Binary

queryTxThatConsumes
  :: SendPort ChainSyncQuery
  -> TxOutRef
  -> (TxWithBlockHeader -> Process a)
  -> (MarloweChainPoint -> Process a)
  -> Process a
queryTxThatConsumes sendQuery out onResponseCB onRollbackCB = do
  (onResponse, receiveResponse) <- newChan
  (onRollback, receiveRollback) <- newChan
  sendChan sendQuery $ QueryTxThatConsumes out $ SendPortWithRollback{..}
  receiveWait [matchChan receiveResponse onResponseCB, matchChan receiveRollback onRollbackCB]

queryBlockNo
  :: SendPort ChainSyncQuery
  -> MarloweBlockNo
  -> (MarloweBlockHeader -> Process a)
  -> (MarloweChainPoint -> Process a)
  -> Process a
queryBlockNo sendQuery blockNo onResponseCB onRollbackCB = do
  (onResponse, receiveResponse) <- newChan
  (onRollback, receiveRollback) <- newChan
  sendChan sendQuery $ QueryBlockNo blockNo $ SendPortWithRollback{..}
  receiveWait [matchChan receiveResponse onResponseCB, matchChan receiveRollback onRollbackCB]

queryIntersectionPoints :: SendPort ChainSyncQuery -> Process [MarloweChainPoint]
queryIntersectionPoints sendQuery = do
  (sendPort, receiveResponse) <- newChan
  sendChan sendQuery $ QueryIntersectionPoints sendPort
  receiveChan receiveResponse

chainSyncClient :: ChainSyncClientDependencies -> Process ()
chainSyncClient ChainSyncClientDependencies{..} = do
  let ChainSyncClientConfig{..} = config
  intersectionPoints <- queryIntersectionPoints sendQuery
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
