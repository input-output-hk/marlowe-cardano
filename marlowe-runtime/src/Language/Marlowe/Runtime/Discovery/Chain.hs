{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Language.Marlowe.Runtime.Discovery.Chain
  where

import Control.Concurrent.Component
import Control.Concurrent.STM (STM, atomically, newTQueue, readTQueue, writeTQueue)
import Control.Monad (guard)
import Data.Coerce (coerce)
import Data.Crosswalk (crosswalk)
import Data.Foldable (fold, for_)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import Data.Void (Void)
import qualified Language.Marlowe.Core.V1.Semantics as V1
import Language.Marlowe.Runtime.ChainSync.Api (FindTxsToError)
import qualified Language.Marlowe.Runtime.ChainSync.Api as Chain
import Language.Marlowe.Runtime.Core.Api
  (ContractId(..), MarloweVersion(..), SomeMarloweVersion(..), fromChainDatum, withSomeMarloweVersion)
import Language.Marlowe.Runtime.Core.ScriptRegistry (MarloweScripts(..), getMarloweVersion, getScripts)
import Language.Marlowe.Runtime.Discovery.Api (ContractHeader(..))
import Network.Protocol.Driver (RunClient)
import Observe.Event (EventBackend, addField, finalize, newEvent, withEvent)
import Observe.Event.DSL (FieldSpec(..), SelectorSpec(..))
import Observe.Event.Render.JSON.DSL.Compile (compile)
import Observe.Event.Syntax ((≔))
import qualified Plutus.V1.Ledger.Api as P

type Versions = [Text]
type TransactionSet = Set Chain.Transaction

compile $ SelectorSpec ["discovery", "chain", "client"]
  [ "connect" ≔ ''Void
  , ["handshake", "failed"] ≔ ''Versions
  , ["handshake", "confirmed"] ≔ ''Void
  , "wait" ≔ ''Void
  , ["query", "rejected"] ≔ ''FindTxsToError
  , ["roll", "forward", "to", "genesis"] ≔ ''Void
  , ["roll", "forward"] ≔ FieldSpec ["roll", "forward"]
      [ "block" ≔ ''Chain.BlockHeader
      , "tip" ≔ ''Chain.ChainPoint
      , "results" ≔ ''TransactionSet
      ]
  , ["roll", "backward"] ≔ ''Chain.ChainPoint
  ]

data ChainEvent
  = RolledForward Chain.BlockHeader (Set ContractHeader)
  | RolledBackward Chain.ChainPoint

data DiscoveryChainClientDependencies r = DiscoveryChainClientDependencies
  { connectToChainSeek :: RunClient IO Chain.RuntimeChainSeekClient
  , eventBackend :: EventBackend IO r DiscoveryChainClientSelector
  }

discoveryChainClient :: Component IO (DiscoveryChainClientDependencies r) (STM ChainEvent)
discoveryChainClient = component \DiscoveryChainClientDependencies{..} -> do
  queue <- newTQueue
  let
    clientInit = Chain.SendMsgRequestHandshake Chain.moveSchema clientHandshake
    clientHandshake = Chain.ClientStHandshake
      { recvMsgHandshakeRejected = \versions -> withEvent eventBackend HandshakeFailed \ev -> do
          addField ev $ coerce versions
          error
            $ "ChainSeek handshake failed. Requested schema version "
            <> show Chain.moveSchema
            <> ", requires "
            <> show versions
            <> "."
      , recvMsgHandshakeConfirmed = withEvent eventBackend HandshakeConfirmed $ const $ pure clientIdle
      }

    clientIdle = Chain.SendMsgQueryNext
      (Chain.FindTxsTo $ Set.map Chain.ScriptCredential marloweScriptHashes) clientNext do
        withEvent eventBackend Wait $ const $ pure clientNext

    clientNext = Chain.ClientStNext
      { recvMsgQueryRejected = \err _ -> withEvent eventBackend QueryRejected \ev -> do
          addField ev err
          pure clientIdle
      , recvMsgRollForward = \txs -> \case
          Chain.Genesis -> \_ ->
            withEvent eventBackend RollForwardToGenesis $ const $ error "Roll forward to Genesis"
          Chain.At block -> \tip -> withEvent eventBackend RollForward \ev -> do
            addField ev $ Block block
            addField ev $ Tip tip
            addField ev $ Results txs
            atomically
              $ for_ (fmap fold $ crosswalk (extractHeaders block) $ Set.toList txs)
              $ writeTQueue queue . RolledForward block
            pure clientIdle
      , recvMsgRollBackward = \point _ -> withEvent eventBackend RollBackward \ev -> do
          addField ev point
          atomically $ writeTQueue queue $ RolledBackward point
          pure clientIdle
      }

  pure
    ( do
        ev <- newEvent eventBackend Connect
        connectToChainSeek $ Chain.ChainSeekClient do
          finalize ev
          pure clientInit
    , readTQueue queue
    )

extractHeaders :: Chain.BlockHeader -> Chain.Transaction -> Maybe (Set ContractHeader)
extractHeaders blockHeader Chain.Transaction{..} =
  Set.fromList <$> crosswalk (uncurry extractHeader) (zip [0..] outputs)
  where
    isMarloweAddress address = case Chain.paymentCredential address of
      Just (Chain.ScriptCredential hash) -> Set.member hash marloweScriptHashes
      _ -> False
    isMarloweInput Chain.TransactionInput{address} = isMarloweAddress address
    hasMarloweInput = any isMarloweInput inputs
    extractHeader ix Chain.TransactionOutput{..} = do
      guard $ not hasMarloweInput
      credential <- Chain.paymentCredential address
      marloweScriptHash <- case credential of
        Chain.ScriptCredential hash -> pure hash
        _ -> Nothing
      (SomeMarloweVersion version, MarloweScripts{..}) <- getMarloweVersion marloweScriptHash
      marloweDatum <- fromChainDatum version =<< datum
      let
        rolesCurrency = case version of
          MarloweV1 -> Chain.PolicyId
            $ P.fromBuiltin
            $ P.unCurrencySymbol
            $ V1.rolesCurrency
            $ V1.marloweParams marloweDatum
      pure ContractHeader
        { contractId = ContractId $ Chain.TxOutRef txId $ fromInteger ix
        , rolesCurrency
        , metadata = mempty -- TODO we need to index metadata in chain sync!
        , marloweScriptHash
        , marloweScriptAddress = address
        , payoutScriptHash = payoutScript
        , marloweVersion = SomeMarloweVersion version
        , blockHeader
        }

marloweScriptHashes :: Set Chain.ScriptHash
marloweScriptHashes = Set.map marloweScript $ foldMap @[] (withSomeMarloweVersion getScripts) [minBound..maxBound]
