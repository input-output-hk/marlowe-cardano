{-# LANGUAGE BlockArguments            #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DeriveAnyClass            #-}
{-# LANGUAGE DerivingStrategies        #-}
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE NamedFieldPuns            #-}
{-# LANGUAGE OverloadedLists           #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE TypeApplications          #-}
module History.Digest.Worker where

import ChainSync.Client (ChainSyncQuery (..), TxWithBlockHeader (..), queryBlockNo, queryTxThatConsumes)
import Control.Distributed.Process (Closure, Process, SendPort, say, sendChan)
import Control.Distributed.Process.Closure (remotable)
import Control.Distributed.Process.Internal.Closure.TH (mkClosure)
import Data.Binary (Binary, Word64)
import Data.Data (Typeable)
import Data.Foldable (traverse_)
import Data.List.NonEmpty (NonEmpty ((:|)))
import GHC.Generics (Generic)
import Language.Marlowe.Runtime.Chain.Types (MarloweBlockHeader (..), MarloweBlockNo (..), MarloweChainPoint (..),
                                             MarloweSlotNo (..), MarloweTxId (..), MarloweTxOut (..))
import Language.Marlowe.Runtime.History (extractEvents)
import Language.Marlowe.Runtime.History.Types (AppTxOutRef (..), ContractCreationTxOut (..), Event (..))

securityParam :: Word64
securityParam = 2160

data Error
  = ExtractFailed MarloweTxId String
  | DivergentHistory AppTxOutRef AppTxOutRef

data HistoryDigestWorkerDependencies = HistoryDigestWorkerDependencies
  { creation  :: ContractCreationTxOut
  , sendEvent :: SendPort Event
  , sendQuery :: SendPort ChainSyncQuery
  }
  deriving (Generic, Typeable, Show, Eq)
  deriving anyclass Binary

data AppTxOutRefWithBlockHeader = AppTxOutRefWithBlockHeader
  { blockHeader :: MarloweBlockHeader
  , ref         :: AppTxOutRef
  }
  deriving Show

type State = NonEmpty AppTxOutRefWithBlockHeader

digestWorker :: HistoryDigestWorkerDependencies -> Process ()
digestWorker HistoryDigestWorkerDependencies{..} = do
  work initialState
  where
    ContractCreationTxOut{..} = creation
    validatorAddress = marloweTxOut_address txOut
    initialState = [AppTxOutRefWithBlockHeader header $ AppTxOutRef (marloweTxOut_txOutRef txOut) datum]

    handleRollback :: NonEmpty AppTxOutRefWithBlockHeader -> MarloweChainPoint -> Process ()
    handleRollback _ MarloweChainPointAtGenesis       = pure ()
    handleRollback history (MarloweChainPoint slot _) = handleRollbackToSlot history slot

    handleRollbackToSlot :: NonEmpty AppTxOutRefWithBlockHeader -> MarloweSlotNo -> Process ()
    handleRollbackToSlot history@(AppTxOutRefWithBlockHeader (MarloweBlockHeader utxoSlot _ _) _ :| prevUtxos) slot
      | slot >= utxoSlot = work history
      | otherwise = case prevUtxos of
          []   -> pure ()
          x:xs -> handleRollbackToSlot (x :| xs) slot

    work :: NonEmpty AppTxOutRefWithBlockHeader -> Process ()
    work history@(prevUtxo@(AppTxOutRefWithBlockHeader _ appOut) :| prevTxos) = do
      let AppTxOutRef out _ = appOut
      queryTxThatConsumes sendQuery out handleResponse $ handleRollback history
        where
          handleResponse TxWithBlockHeader{..} = do
            case extractEvents validatorAddress contractId appOut blockHeader tx of
              Left err -> do
                say $ "error: " <> err
                let MarloweBlockHeader _ _ blockNo = blockHeader
                retire blockNo history
              Right (mNewAppOut, events) -> do
                traverse_ (sendChan sendEvent) events
                case mNewAppOut of
                  Nothing        -> let MarloweBlockHeader _ _ blockNo = blockHeader in retire blockNo history
                  Just newAppOut -> work $ AppTxOutRefWithBlockHeader blockHeader newAppOut :| prevUtxo : prevTxos

    retire (MarloweBlockNo blockNo) history = do
      let endOfLifeBlock = MarloweBlockNo $ blockNo + securityParam
      queryBlockNo sendQuery endOfLifeBlock (const $ pure ()) $ handleRollback history

remotable ['digestWorker]

process :: HistoryDigestWorkerDependencies -> Closure (Process ())
process = $(mkClosure 'digestWorker)
