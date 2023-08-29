{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}

module Language.Marlowe.Runtime.Transaction.Query where

import Cardano.Api (NetworkId)
import qualified Cardano.Api as C
import Control.Error (hush)
import Control.Error.Util (note)
import Control.Monad.Event.Class
import Control.Monad.Trans.Class (lift)
import Data.Foldable (find)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as Map
import Data.Maybe (fromJust, mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Type.Equality (testEquality, type (:~:) (Refl))
import Data.Void (Void)
import Language.Marlowe.Runtime.Cardano.Api
import Language.Marlowe.Runtime.ChainSync.Api (
  Address,
  BlockHeader,
  ChainPoint,
  ChainSyncQuery (..),
  Credential (..),
  GetUTxOsQuery (..),
  Move (..),
  RuntimeChainSeekClient,
  ScriptHash,
  TxOutRef (TxOutRef),
  UTxOs (..),
  WithGenesis (..),
  paymentCredential,
 )
import Language.Marlowe.Runtime.Core.Api (
  ContractId (..),
  MarloweVersion,
  SomeMarloweVersion (..),
  TransactionScriptOutput (..),
 )
import qualified Language.Marlowe.Runtime.Core.Api as Core
import Language.Marlowe.Runtime.Core.ScriptRegistry (MarloweScripts (..), ReferenceScriptUtxo)
import Language.Marlowe.Runtime.History.Api (
  CreateStep (..),
  ExtractCreationError,
  ExtractMarloweTransactionError,
  SomeCreateStep (..),
  extractCreation,
  extractMarloweTransaction,
 )
import Language.Marlowe.Runtime.Transaction.Api
import Language.Marlowe.Runtime.Transaction.Constraints
import Network.Protocol.ChainSeek.Client
import Network.Protocol.Connection (Connector, runConnector)
import Network.Protocol.Query.Client (QueryClient, request)
import Observe.Event.Explicit (addField)
import UnliftIO (MonadUnliftIO)

data LoadWalletContextSelector f where
  LoadWalletContext :: LoadWalletContextSelector LoadWalletContextField

data LoadWalletContextField
  = ForAddresses (Set Address)
  | WalletContextLoaded WalletContext

data LoadPayoutContextSelector f where
  LoadPayoutContext :: LoadPayoutContextSelector LoadPayoutContextField

data LoadPayoutContextField
  = ForPayouts (Set TxOutRef)
  | PayoutContextLoaded PayoutContext

data LoadMarloweContextSelector f where
  LoadMarloweContext :: LoadMarloweContextSelector LoadMarloweContextField
  ContractNotFound :: LoadMarloweContextSelector Void
  ContractFound :: LoadMarloweContextSelector ContractFoundField
  ExtractCreationFailed :: LoadMarloweContextSelector ExtractCreationError
  ExtractMarloweTransactionFailed :: LoadMarloweContextSelector ExtractMarloweTransactionError
  ContractTipFound :: MarloweVersion v -> LoadMarloweContextSelector (MarloweContext v)

data LoadMarloweContextField where
  DesiredVersion :: MarloweVersion v -> LoadMarloweContextField
  Contract :: ContractId -> LoadMarloweContextField

data ContractFoundField where
  ActualVersion :: MarloweVersion v -> ContractFoundField
  MarloweScriptAddress :: Address -> ContractFoundField
  PayoutScriptHash :: ScriptHash -> ContractFoundField

type LoadWalletContext m = WalletAddresses -> m WalletContext

type LoadPayoutContext m =
  forall v
   . MarloweVersion v
  -> Set TxOutRef
  -> m PayoutContext

type LoadMarloweContext m =
  forall v
   . MarloweVersion v
  -> ContractId
  -> m (Either LoadMarloweContextError (MarloweContext v))

loadWalletContext
  :: (MonadInjectEvent r LoadWalletContextSelector s m)
  => (GetUTxOsQuery -> m UTxOs)
  -> LoadWalletContext m
loadWalletContext runQuery WalletAddresses{..} =
  withEvent LoadWalletContext \ev -> do
    let addresses = Set.insert changeAddress extraAddresses
    addField ev $ ForAddresses addresses
    availableUtxos@(UTxOs (Map.keys -> txOutRefs)) <- runQuery $ GetUTxOsAtAddresses addresses
    let collateralUtxos' = Set.filter (flip elem txOutRefs) collateralUtxos
        walletContext =
          WalletContext
            { availableUtxos = availableUtxos
            , collateralUtxos = collateralUtxos'
            , changeAddress = changeAddress
            }
    addField ev $ WalletContextLoaded walletContext
    pure walletContext

loadPayoutContext
  :: (MonadInjectEvent r LoadPayoutContextSelector s m)
  => (forall v. MarloweVersion v -> Set MarloweScripts)
  -> C.NetworkId
  -> (GetUTxOsQuery -> m UTxOs)
  -> LoadPayoutContext m
loadPayoutContext getScripts networkId runQuery version payouts =
  withEvent LoadPayoutContext \ev -> do
    addField ev $ ForPayouts payouts
    UTxOs payoutOutputs <- runQuery $ GetUTxOsForTxOutRefs payouts
    let payoutContext =
          PayoutContext
            { payoutOutputs
            , payoutScriptOutputs =
                Map.fromList
                  . mapMaybe \case
                    MarloweScripts{..} -> (payoutScript,) <$> Map.lookup networkId payoutScriptUTxOs
                  . Set.toList
                  $ getScripts version
            }
    addField ev $ PayoutContextLoaded payoutContext
    pure payoutContext

-- | Loads the current MarloweContext for a contract by its ID.
loadMarloweContext
  :: forall m r s
   . (MonadUnliftIO m, MonadInjectEvent r LoadMarloweContextSelector s m)
  => (forall v. MarloweVersion v -> Set MarloweScripts)
  -> C.NetworkId
  -> Connector RuntimeChainSeekClient m
  -> Connector (QueryClient ChainSyncQuery) m
  -> LoadMarloweContext m
loadMarloweContext getScripts networkId chainSyncConnector chainSyncQueryConnector desiredVersion contractId =
  withEventFields LoadMarloweContext [DesiredVersion desiredVersion, Contract contractId] $
    const $
      runConnector chainSyncConnector client
  where
    TxOutRef creationTxId _ = unContractId contractId
    client = ChainSeekClient $ pure clientFindContract

    clientFindContract =
      SendMsgQueryNext
        (FindTx creationTxId False)
        ClientStNext
          { recvMsgQueryRejected = \_ _ -> do
              emitImmediateEvent_ ContractNotFound
              pure $ SendMsgDone $ Left LoadMarloweContextErrorNotFound
          , recvMsgRollBackward = \_ _ -> pure clientFindContract
          , recvMsgRollForward = \tx point _ -> case point of
              Genesis -> error "Roll forward to Genesis"
              At blockHeader -> do
                case extractCreation contractId tx of
                  Left e -> do
                    emitImmediateEventFields_ ExtractCreationFailed [e]
                    pure $ SendMsgDone $ Left $ ExtractCreationError e
                  Right (SomeCreateStep actualVersion CreateStep{..}) -> do
                    let TransactionScriptOutput{..} = createOutput
                    let marloweScriptHash =
                          fromJust $
                            paymentCredential address >>= \case
                              ScriptCredential hash -> pure hash
                              _ -> Nothing
                        matchesScriptHash MarloweScripts{..} = marloweScript == marloweScriptHash
                        scripts = getScripts actualVersion
                        marloweScripts = fromJust $ find matchesScriptHash scripts
                    let marloweScriptUTxO = fromJust $ hush $ lookupMarloweScriptUtxo networkId marloweScripts
                    let payoutScriptUTxO = fromJust $ hush $ lookupPayoutScriptUtxo networkId marloweScripts
                    case testEquality desiredVersion actualVersion of
                      Nothing ->
                        pure $
                          SendMsgDone $
                            Left $
                              LoadMarloweContextErrorVersionMismatch $
                                SomeMarloweVersion actualVersion
                      Just Refl -> do
                        emitImmediateEventFields_
                          ContractFound
                          [ ActualVersion actualVersion
                          , MarloweScriptAddress address
                          , PayoutScriptHash payoutValidatorHash
                          ]
                        pure $
                          clientFollowContract actualVersion $
                            pure
                              ( blockHeader
                              , MarloweContext
                                  { marloweAddress = address
                                  , payoutScriptHash = payoutValidatorHash
                                  , marloweScriptHash =
                                      fromJust $
                                        paymentCredential address >>= \case
                                          ScriptCredential hash -> pure hash
                                          _ -> Nothing
                                  , payoutAddress =
                                      fromCardanoAddressInEra C.BabbageEra $
                                        C.AddressInEra (C.ShelleyAddressInEra C.ShelleyBasedEraBabbage) $
                                          C.makeShelleyAddress
                                            networkId
                                            (C.PaymentCredentialByScript $ fromJust $ toCardanoScriptHash payoutValidatorHash)
                                            C.NoStakeAddress
                                  , -- Get the script output of the create event.
                                    scriptOutput = Just createOutput
                                  , marloweScriptUTxO
                                  , payoutScriptUTxO
                                  }
                              )
          , recvMsgWait = do
              emitImmediateEvent_ ContractNotFound
              pure $ SendMsgCancel $ SendMsgDone $ Left LoadMarloweContextErrorNotFound
          }

    clientFollowContract
      :: forall v
       . MarloweVersion v
      -> NonEmpty (BlockHeader, MarloweContext v)
      -> ClientStIdle Move ChainPoint ChainPoint m (Either LoadMarloweContextError (MarloweContext v))
    clientFollowContract version contexts = case scriptUtxo of
      Nothing -> SendMsgDone $ Left LoadMarloweContextErrorNotFound
      Just lastOutput ->
        SendMsgQueryNext
          (FindConsumingTxs $ Set.singleton lastOutput)
          ClientStNext
            { recvMsgQueryRejected = \_ _ -> do
                emitImmediateEvent_ ContractNotFound
                pure $ SendMsgDone $ Left LoadMarloweContextErrorNotFound
            , recvMsgRollBackward = \point _ -> case rollbackContexts point contexts of
                Nothing -> do
                  emitImmediateEvent_ ContractNotFound
                  pure $ SendMsgDone $ Left LoadMarloweContextErrorNotFound
                Just contexts' -> pure $ clientFollowContract version contexts'
            , recvMsgRollForward = \txs point _ -> case point of
                Genesis -> error "Roll forward to Genesis"
                At blockHeader -> case scriptUtxo >>= \u -> (u,) <$> Map.lookup u txs of
                  Nothing -> pure $ clientFollowContract version $ updateContext blockHeader Nothing contexts
                  Just (u, scriptConsumer) -> runConnector chainSyncQueryConnector do
                    systemStart <- request GetSystemStart
                    eraHistory <- request GetEraHistory
                    lift case extractMarloweTransaction
                      version
                      systemStart
                      eraHistory
                      contractId
                      marloweAddress
                      payoutScriptHash
                      u
                      blockHeader
                      scriptConsumer of
                      Left e -> do
                        emitImmediateEventFields_ ExtractMarloweTransactionFailed [e]
                        pure $ SendMsgDone $ Left $ ExtractMarloweTransactionError e
                      Right marloweTransaction -> pure $ clientFollowContract version $ updateContext blockHeader (Just marloweTransaction) contexts
            , recvMsgWait = do
                emitImmediateEventFields_ (ContractTipFound version) [context]
                pure $ SendMsgCancel $ SendMsgDone $ Right context
            }
      where
        context@MarloweContext{..} = snd $ NE.head contexts
        scriptUtxo = utxo <$> scriptOutput

    updateContext
      :: BlockHeader
      -> Maybe (Core.Transaction v)
      -> NonEmpty (BlockHeader, MarloweContext v)
      -> NonEmpty (BlockHeader, MarloweContext v)
    updateContext blockHeader mTransaction ((bh, context) :| contexts') =
      ( blockHeader
      , context
          { scriptOutput = maybe (scriptOutput context) (Core.scriptOutput . Core.output) mTransaction
          }
      )
        :| ((bh, context) : contexts')

    rollbackContexts
      :: ChainPoint
      -> NonEmpty (BlockHeader, MarloweContext v1)
      -> Maybe (NonEmpty (BlockHeader, MarloweContext v1))
    rollbackContexts Genesis _ = Nothing
    rollbackContexts (At block) contexts = case dropWhile ((block <) . fst) $ NE.toList contexts of
      [] -> Nothing
      x : xs -> Just $ x :| xs

lookupMarloweScriptUtxo :: NetworkId -> MarloweScripts -> Either LoadMarloweContextError ReferenceScriptUtxo
lookupMarloweScriptUtxo networkId MarloweScripts{..} = note (MarloweScriptNotPublished marloweScript) $ Map.lookup networkId marloweScriptUTxOs

lookupPayoutScriptUtxo :: NetworkId -> MarloweScripts -> Either LoadMarloweContextError ReferenceScriptUtxo
lookupPayoutScriptUtxo networkId MarloweScripts{..} = note (PayoutScriptNotPublished payoutScript) $ Map.lookup networkId payoutScriptUTxOs
