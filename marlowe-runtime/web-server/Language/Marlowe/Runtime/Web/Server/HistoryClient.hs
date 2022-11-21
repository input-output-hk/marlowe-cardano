{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Language.Marlowe.Runtime.Web.Server.HistoryClient
  where

import Control.Concurrent.STM (STM)
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Void (Void)
import Language.Marlowe.Protocol.Sync.Client
import qualified Language.Marlowe.Runtime.ChainSync.Api as Chain
import Language.Marlowe.Runtime.Core.Api
  (ContractId, MarloweVersion, Transaction(..), TransactionOutput(..), TransactionScriptOutput)
import Language.Marlowe.Runtime.History.Api (ContractStep(..), CreateStep(..))
import Language.Marlowe.Runtime.Web.Server.DTO (ContractRecord(..))
import Observe.Event (EventBackend, addField, withEvent)
import Observe.Event.BackendModification (EventBackendModifiers, modifyEventBackend)
import Observe.Event.DSL (SelectorSpec(..))
import Observe.Event.Render.JSON.DSL.Compile (compile)
import Observe.Event.Syntax ((≔))

compile $ SelectorSpec ["history", "client"]
  [ ["load", "contract", "not", "found"] ≔ ''Void
  , ["load", "contract", "found"] ≔ ''String
  , ["load", "contract", "roll", "forward"] ≔ ''String
  , ["load", "contract", "roll", "backward"] ≔ ''String
  , ["load", "contract", "roll", "back", "creation"] ≔ ''Void
  , ["load", "contract", "wait"] ≔ ''Void
  ]

data HistoryClientDependencies r = HistoryClientDependencies
  { runMarloweSyncClient :: forall a. MarloweSyncClient IO a -> IO a
  , eventBackend :: EventBackend IO r HistoryClientSelector
  }

-- | Signature for a delegate that loads the state of a single contract.
type LoadContract r m
   = forall r'
   . EventBackendModifiers r r'
  -> ContractId               -- ^ ID of the contract to load
  -> m (Maybe ContractRecord) -- ^ Nothing if the ID is not found

-- | Public API of the HistoryClient
newtype HistoryClient r = HistoryClient
  { loadContract :: LoadContract r IO -- ^ Load contract headers from the indexer.
  }

mkHistoryClient :: HistoryClientDependencies r -> STM (HistoryClient r)
mkHistoryClient HistoryClientDependencies{..} = pure HistoryClient
  { loadContract = \mods ->
      runMarloweSyncClient . loadContractClient (modifyEventBackend mods eventBackend)
  }

loadContractClient
  :: EventBackend IO r HistoryClientSelector
  -> ContractId
  -> MarloweSyncClient IO (Maybe ContractRecord)
loadContractClient eb contractId = MarloweSyncClient $ pure clientInit
  where
    clientInit = SendMsgFollowContract contractId ClientStFollow
      { recvMsgContractNotFound =
          withEvent eb LoadContractNotFound $ const $ pure Nothing
      , recvMsgContractFound = \block version createStep@CreateStep{..} ->
          withEvent eb LoadContractFound \ev -> do
            addField ev $ show block
            pure $ clientIdle block version createStep createOutput
      }

    clientIdle
      :: Chain.BlockHeader
      -> MarloweVersion v
      -> CreateStep v
      -> TransactionScriptOutput v
      -> ClientStIdle v IO (Maybe ContractRecord)
    clientIdle createBlock version createStep lastOutput = SendMsgRequestNext ClientStNext
      { recvMsgRollForward = \block steps ->
          withEvent eb LoadContractRollForward \ev -> do
            addField ev $ show block
            let
              mLastOutput =  listToMaybe
                $ mapMaybe \case
                    ApplyTransaction Transaction{output=TransactionOutput{..}} -> scriptOutput
                    _ -> Just lastOutput
                $ reverse steps
            pure case mLastOutput of
              Nothing -> SendMsgDone $ Just $ ContractRecord
                version
                contractId
                createBlock
                createStep
                Nothing
              Just lastOutput' -> clientIdle createBlock version createStep lastOutput'
      , recvMsgRollBackward = \block ->
          withEvent eb LoadContractRollBackward \ev -> do
            addField ev $ show block
            pure $ clientIdle createBlock version createStep lastOutput
      , recvMsgRollBackCreation =
          withEvent eb LoadContractRollBackCreation $ const $ pure Nothing
      , recvMsgWait = do
          withEvent eb LoadContractWait
            $ const
            $ pure
            $ SendMsgCancel
            $ SendMsgDone
            $ Just
            $ ContractRecord version contractId createBlock createStep
            $ Just lastOutput
      }
