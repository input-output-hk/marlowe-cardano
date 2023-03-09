{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}

module Language.Marlowe.Runtime.Transaction.Query
  where

import Cardano.Api (NetworkId)
import qualified Cardano.Api as C
import Control.Error (hush)
import Control.Error.Util (note)
import Data.Foldable (find)
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Type.Equality (testEquality, type (:~:)(Refl))
import Data.Void (Void)
import Language.Marlowe.Runtime.Cardano.Api
import Language.Marlowe.Runtime.ChainSync.Api
  ( Address
  , BlockHeader
  , ChainPoint
  , ChainSyncQuery(..)
  , Credential(..)
  , GetUTxOsQuery(..)
  , Move(..)
  , RuntimeChainSeekClient
  , ScriptHash
  , Transaction(..)
  , TxOutRef(TxOutRef)
  , UTxOs(..)
  , WithGenesis(..)
  , paymentCredential
  )
import Language.Marlowe.Runtime.Core.Api
  (ContractId(..), MarloweVersion, SomeMarloweVersion(..), TransactionScriptOutput(..))
import qualified Language.Marlowe.Runtime.Core.Api as Core
import Language.Marlowe.Runtime.Core.ScriptRegistry (MarloweScripts(..), ReferenceScriptUtxo)
import Language.Marlowe.Runtime.History.Api
  ( CreateStep(..)
  , ExtractCreationError
  , ExtractMarloweTransactionError
  , SomeCreateStep(..)
  , extractCreation
  , extractMarloweTransaction
  )
import Language.Marlowe.Runtime.Transaction.Api
import Language.Marlowe.Runtime.Transaction.Constraints
import Network.Protocol.ChainSeek.Client
import Network.Protocol.Connection (SomeClientConnector)
import Network.Protocol.Driver (runSomeConnector)
import Network.Protocol.Query.Client (QueryClient, liftQuery)
import Observe.Event.Explicit (EventBackend, addField, idInjectSelector, subEventBackend, withEvent)

data LoadWalletContextSelector f where
  LoadWalletContext :: LoadWalletContextSelector LoadWalletContextField

data LoadWalletContextField
  = ForAddresses (Set Address)
  | WalletContextLoaded WalletContext

data LoadMarloweContextSelector f where
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
  ContractUTxO :: TxOutRef -> ContractFoundField

type LoadWalletContext r
   = EventBackend IO r LoadWalletContextSelector
  -> WalletAddresses
  -> IO WalletContext

type LoadMarloweContext r
   = forall v
   . EventBackend IO r LoadMarloweContextSelector
  -> MarloweVersion v
  -> ContractId
  -> IO (Either LoadMarloweContextError (MarloweContext v))

loadWalletContext :: (GetUTxOsQuery -> IO UTxOs) -> LoadWalletContext r
loadWalletContext runQuery eventBackend WalletAddresses{..} =
  withEvent eventBackend LoadWalletContext \ev -> do
    let addresses = Set.insert changeAddress extraAddresses
    addField ev $ ForAddresses addresses
    availableUtxos@(UTxOs (Map.keys -> txOutRefs)) <- runQuery $ GetUTxOsAtAddresses addresses
    let
      collateralUtxos' = Set.filter (flip elem txOutRefs) collateralUtxos
      walletContext = WalletContext
        { availableUtxos=availableUtxos
        , collateralUtxos=collateralUtxos'
        , changeAddress=changeAddress
        }
    addField ev $ WalletContextLoaded walletContext
    pure walletContext

-- | Loads the current MarloweContext for a contract by its ID.
loadMarloweContext
  :: (forall v. MarloweVersion v -> Set MarloweScripts)
  -> C.NetworkId
  -> SomeClientConnector RuntimeChainSeekClient IO
  -> SomeClientConnector (QueryClient ChainSyncQuery) IO
  -> LoadMarloweContext r
loadMarloweContext getScripts networkId chainSyncConnector chainSyncQueryConnector eventBackend desiredVersion contractId =
  runSomeConnector chainSyncConnector client
  where
    TxOutRef creationTxId _ = unContractId contractId
    client = ChainSeekClient $ pure clientFindContract

    clientFindContract = SendMsgQueryNext (FindTx creationTxId False) ClientStNext
      { recvMsgQueryRejected = \_ _ -> withEvent eventBackend ContractNotFound
          $ const
          $ pure
          $ SendMsgDone
          $ Left LoadMarloweContextErrorNotFound
      , recvMsgRollBackward = \_ _ -> pure clientFindContract
      , recvMsgRollForward = \tx point _ -> case point of
          Genesis -> error "Roll forward to Genesis"
          At blockHeader -> withEvent eventBackend ContractFound \ev -> case extractCreation contractId tx of
            Left e -> withEvent (subEventBackend idInjectSelector ev eventBackend) ExtractCreationFailed \ev' -> do
              addField ev' e
              pure $ SendMsgDone $ Left $ ExtractCreationError e
            Right (SomeCreateStep actualVersion CreateStep{..}) -> do
              let TransactionScriptOutput{..} = createOutput
              let
                marloweScriptHash = fromJust $ paymentCredential address >>= \case
                  ScriptCredential hash -> pure hash
                  _ -> Nothing
                matchesScriptHash MarloweScripts{..} = marloweScript == marloweScriptHash
                scripts = getScripts actualVersion
                marloweScripts = fromJust $ find matchesScriptHash scripts
              let marloweScriptUTxO = fromJust $ hush $ lookupMarloweScriptUtxo networkId marloweScripts
              let payoutScriptUTxO = fromJust $ hush $ lookupPayoutScriptUtxo networkId marloweScripts
              pure case testEquality desiredVersion actualVersion of
                Nothing -> SendMsgDone
                  $ Left
                  $ LoadMarloweContextErrorVersionMismatch
                  $ SomeMarloweVersion actualVersion
                Just Refl -> clientFollowContract actualVersion $ pure
                  ( blockHeader
                  , MarloweContext
                    { marloweAddress = address
                    , payoutScriptHash = payoutValidatorHash
                    , marloweScriptHash = fromJust $ paymentCredential address >>= \case
                        ScriptCredential hash -> pure hash
                        _ -> Nothing
                    , payoutAddress = fromCardanoAddressInEra C.BabbageEra
                        $ C.AddressInEra (C.ShelleyAddressInEra C.ShelleyBasedEraBabbage)
                        $ C.makeShelleyAddress
                            networkId
                            (C.PaymentCredentialByScript $ fromJust $ toCardanoScriptHash payoutValidatorHash)
                            C.NoStakeAddress
                    -- Get the script output of the create event.
                    , scriptOutput = Just createOutput
                    -- No payouts to start with
                    , payoutOutputs = mempty
                    , marloweScriptUTxO
                    , payoutScriptUTxO
                    }
                  )
      , recvMsgWait = withEvent eventBackend ContractNotFound
          $ const
          $ pure
          $ SendMsgCancel
          $ SendMsgDone
          $ Left LoadMarloweContextErrorNotFound
      }

    clientFollowContract
      :: forall v
       . MarloweVersion v
      -> NonEmpty (BlockHeader, MarloweContext v)
      -> ClientStIdle Move ChainPoint ChainPoint IO (Either LoadMarloweContextError (MarloweContext v))
    clientFollowContract version contexts = SendMsgQueryNext (FindConsumingTxs utxos) ClientStNext
      { recvMsgQueryRejected = \_ _ -> withEvent eventBackend ContractNotFound
          $ const
          $ pure
          $ SendMsgDone
          $ Left LoadMarloweContextErrorNotFound
      , recvMsgRollBackward = \point _ -> case rollbackContexts point contexts of
          Nothing -> withEvent eventBackend ContractNotFound
            $ const
            $ pure
            $ SendMsgDone
            $ Left LoadMarloweContextErrorNotFound
          Just contexts' -> pure $ clientFollowContract version contexts'
      , recvMsgRollForward = \txs point _ -> case point of
          Genesis -> error "Roll forward to Genesis"
          At blockHeader -> case scriptUtxo >>= \u -> (u,) <$> Map.lookup u txs of
            Nothing -> pure $ clientFollowContract version $ updateContext blockHeader Nothing txs contexts
            Just (u, scriptConsumer) -> do
              systemStart <- fromJust . hush <$> runSomeConnector chainSyncQueryConnector (liftQuery GetSystemStart)
              eraHistory <- fromJust . hush <$> runSomeConnector chainSyncQueryConnector (liftQuery GetEraHistory)
              case extractMarloweTransaction version systemStart eraHistory contractId marloweAddress payoutScriptHash u blockHeader scriptConsumer of
                Left e -> withEvent eventBackend ExtractMarloweTransactionFailed \ev' -> do
                  addField ev' e
                  pure $ SendMsgDone $ Left $ ExtractMarloweTransactionError e
                Right marloweTransaction -> pure $ clientFollowContract version $ updateContext blockHeader (Just marloweTransaction) txs contexts
      , recvMsgWait = withEvent eventBackend (ContractTipFound version) \ev -> do
          addField ev context
          pure $ SendMsgCancel $ SendMsgDone $ Right context
      }
      where
        context@MarloweContext{..} = snd $ NE.head contexts
        scriptUtxo = utxo <$> scriptOutput
        utxos = maybe id Set.insert scriptUtxo $ Map.keysSet payoutOutputs

    updateContext
      :: BlockHeader
      -> Maybe (Core.Transaction v)
      -> Map.Map TxOutRef Transaction
      -> NonEmpty (BlockHeader, MarloweContext v)
      -> NonEmpty (BlockHeader, MarloweContext v)
    updateContext blockHeader mTransaction txs ((bh, context) :| contexts') =
      ( blockHeader
      , context
          { scriptOutput = maybe (scriptOutput context) (Core.scriptOutput . Core.output) mTransaction
          , payoutOutputs = Map.withoutKeys
              (maybe id (Map.union . Core.payouts . Core.output) mTransaction $ payoutOutputs context)
              (Map.keysSet txs)
          }
      ) :| ((bh, context) : contexts')


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
