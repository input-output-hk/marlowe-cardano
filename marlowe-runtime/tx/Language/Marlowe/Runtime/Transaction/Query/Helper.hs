{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Language.Marlowe.Runtime.Transaction.Query.Helper (
  LoadHelpersContext,
  LoadHelpersContextSelector (..),
  loadHelpersContext,
) where

import qualified Cardano.Api as C
import Control.Monad.Event.Class (
  MonadInjectEvent,
  emitImmediateEventFields_,
  emitImmediateEvent_,
  withEventFields,
 )
import qualified Data.Map as Map
import Data.Maybe (fromJust, mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Type.Equality (testEquality, type (:~:) (Refl))
import Data.Void (Void)
import qualified Language.Marlowe.Core.V1.Semantics as V1
import Language.Marlowe.Runtime.Cardano.Api (
  fromCardanoAddressInEra,
  toCardanoScriptHash,
 )
import Language.Marlowe.Runtime.ChainSync.Api (
  AssetId (..),
  Assets (..),
  ChainPoint,
  Move (FindConsumingTxs, FindTx),
  PolicyId,
  RuntimeChainSeekClient,
  TokenName,
  Tokens (..),
  Transaction (Transaction, outputs, txId),
  TransactionOutput (..),
  TxId,
  TxOutRef (TxOutRef),
  WithGenesis (At, Genesis),
 )
import Language.Marlowe.Runtime.Core.Api (
  ContractId (ContractId),
  MarloweVersion (..),
  SomeMarloweVersion (SomeMarloweVersion),
  TransactionScriptOutput (TransactionScriptOutput, datum),
 )
import Language.Marlowe.Runtime.Core.ScriptRegistry (
  MarloweScripts (..),
 )
import Language.Marlowe.Runtime.History.Api (
  CreateStep (CreateStep, createOutput),
  ExtractCreationError,
  SomeCreateStep (SomeCreateStep),
  extractCreation,
 )
import Language.Marlowe.Runtime.Plutus.V2.Api (fromPlutusCurrencySymbol)
import Language.Marlowe.Runtime.Transaction.Api (
  Destination (..),
  HelperScript (..),
  LoadHelpersContextError (..),
  RoleTokensConfig (..),
  unMint,
 )
import Language.Marlowe.Runtime.Transaction.Constraints (
  HelperScriptInfo (..),
  HelperScriptState (..),
  HelpersContext (..),
 )
import Network.Protocol.ChainSeek.Client (
  ChainSeekClient (ChainSeekClient),
  ClientStIdle (SendMsgDone, SendMsgQueryNext),
  ClientStNext (
    ClientStNext,
    recvMsgQueryRejected,
    recvMsgRollBackward,
    recvMsgRollForward,
    recvMsgWait
  ),
  ClientStPoll (..),
 )
import Network.Protocol.Connection (Connector, runConnector)
import Text.Read (readMaybe)
import UnliftIO (MonadUnliftIO)

data LoadHelpersContextSelector f where
  LoadHelpersContext :: LoadHelpersContextSelector ContractId
  ContractNotFound :: LoadHelpersContextSelector Void
  ContractNotExtracted :: LoadHelpersContextSelector ExtractCreationError
  ContractFound :: LoadHelpersContextSelector TxId
  RollForwardToGenesis :: LoadHelpersContextSelector Void
  TxOutRefConsumed :: LoadHelpersContextSelector (TxOutRef, TxId)
  TxOutRefNotConsumed :: LoadHelpersContextSelector TxOutRef
  LoadHelpersContextTxOutRefNotFound :: LoadHelpersContextSelector TxOutRef

type LoadHelpersContext m =
  forall v
   . MarloweVersion v
  -> Either (PolicyId, RoleTokensConfig) (Maybe ContractId)
  -> m (Either LoadHelpersContextError HelpersContext)

loadHelpersContext
  :: forall m r s
   . (MonadUnliftIO m, MonadInjectEvent r LoadHelpersContextSelector s m)
  => (forall v. MarloweVersion v -> MarloweScripts)
  -> (forall v. MarloweVersion v -> Set MarloweScripts)
  -> C.NetworkId
  -> Connector RuntimeChainSeekClient m
  -> LoadHelpersContext m
loadHelpersContext getCurrentScripts _ networkId _ desiredVersion (Right Nothing) =
  pure
    . Right
    $ HelpersContext
      { currentHelperScripts = getHelperInfos helperScript networkId $ getCurrentScripts desiredVersion
      , helperPolicyId = ""
      , helperScriptStates = mempty
      }
loadHelpersContext getCurrentScripts _ networkId _ desiredVersion (Left (rolesCurrency, roleTokens)) =
  -- TODO: Generalize beyoned open roles when other helper script types are supported.
  case (OpenRoleScript `Map.lookup` current, roleTokens) of
    (Just _, RoleTokensNone) ->
      pure . Right $ HelpersContext current rolesCurrency mempty
    (Just _, RoleTokensUsePolicy _) ->
      pure . Right $ HelpersContext current rolesCurrency mempty
    (Just helperScriptInfo, RoleTokensMint mint) ->
      pure
        . Right
        . HelpersContext current rolesCurrency
        . Map.map (const HelperScriptCreateState{..})
        . Map.filter ((== ToScript OpenRoleScript) . fst)
        $ unMint mint
    (Just helperScriptInfo, RoleTokensUsePolicyWithOpenRoles policyId _ openRoleNames) ->
      pure
        . Right
        . HelpersContext current policyId
        . Map.fromList
        $ (,HelperScriptCreateState{..}) <$> openRoleNames
    (Nothing, _) -> pure . Right $ HelpersContext current "" mempty
  where
    current = getHelperInfos helperScript networkId $ getCurrentScripts desiredVersion
loadHelpersContext getCurrentScripts getScripts networkId chainSyncConnector desiredVersion (Right (Just contractId@(ContractId (TxOutRef creationTxId _)))) =
  withEventFields LoadHelpersContext [contractId] $
    const . runConnector chainSyncConnector . ChainSeekClient $
      pure clientFindContract
  where
    clientFindContract :: ClientStIdle Move ChainPoint ChainPoint m (Either LoadHelpersContextError HelpersContext)
    clientFindContract =
      SendMsgQueryNext
        (FindTx creationTxId False)
        ClientStNext
          { recvMsgQueryRejected = \_ _ -> do
              emitImmediateEvent_ ContractNotFound
              -- The creation tx was not found.
              pure $ SendMsgDone $ Left $ LoadHelpersContextErrorNotFound creationTxId
          , recvMsgRollBackward =
              -- Start over if a rollback is encountered.
              \_ _ -> pure clientFindContract
          , recvMsgRollForward = \tx point _ -> case point of
              Genesis -> do
                emitImmediateEvent_ RollForwardToGenesis
                -- Unexpected roll forward to genesis.
                pure $ SendMsgDone $ Left RollForwardToGenesisError
              At _ -> do
                case extractCreation contractId tx of
                  Left e -> do
                    emitImmediateEventFields_ ContractNotExtracted [e]
                    -- The contract could not be parsed.
                    pure . SendMsgDone . Left $ ContractNotExtractedError e
                  Right (SomeCreateStep actualVersion CreateStep{createOutput}) -> do
                    case testEquality desiredVersion actualVersion of
                      Nothing ->
                        -- Wrong contract version encountered.
                        pure . SendMsgDone . Left . LoadHelpersContextErrorVersionMismatch $
                          SomeMarloweVersion actualVersion
                      Just Refl -> do
                        emitImmediateEventFields_ ContractFound [creationTxId]
                        let currentHelperScripts = getHelperInfos helperScript networkId $ getCurrentScripts desiredVersion
                            TransactionScriptOutput{datum} = createOutput
                            scripts = Map.unions $ getHelperInfos helperAddress networkId <$> Set.toList (getScripts desiredVersion)
                            helperPolicyId =
                              case actualVersion of
                                MarloweV1 -> fromPlutusCurrencySymbol . V1.rolesCurrency $ V1.marloweParams datum
                            findHelperScriptOutput (helperTxOutRef, helperTransactionOutput@TransactionOutput{address, assets}) =
                              do
                                roleName <-
                                  case filter ((== helperPolicyId) . policyId) . Map.keys . unTokens . tokens $ assets of
                                    -- Helper scripts never receive more than one role token.
                                    [AssetId{tokenName}] -> pure tokenName
                                    -- Silently ignore script output without exactly one role token: do not through an error
                                    -- because the creation transaction might have be built outside of Runtime.
                                    _ -> Nothing
                                helperScriptInfo <- address `Map.lookup` scripts
                                pure (roleName, HelperScriptState{..})
                            initialStates = mapMaybe findHelperScriptOutput $ zip (TxOutRef creationTxId <$> [0 ..]) $ outputs tx
                            helperScriptStates = mempty
                        pure $ clientFollowHelper HelpersContext{..} initialStates
          , recvMsgWait = do
              emitImmediateEvent_ ContractNotFound
              -- The contract was not found.
              pure . SendMsgCancel . SendMsgDone . Left $ LoadHelpersContextErrorNotFound creationTxId
          }
    clientFollowHelper
      :: HelpersContext
      -> [(TokenName, HelperScriptState)]
      -> ClientStIdle Move ChainPoint ChainPoint m (Either LoadHelpersContextError HelpersContext)
    clientFollowHelper context [] = SendMsgDone $ Right context
    clientFollowHelper context@HelpersContext{helperPolicyId, helperScriptStates} ((roleName, state@HelperScriptState{helperTxOutRef, helperScriptInfo = HelperScriptInfo{helperAddress}}) : remainder) =
      SendMsgQueryNext
        (FindConsumingTxs $ Set.singleton helperTxOutRef)
        ClientStNext
          { recvMsgQueryRejected = \_ _ -> do
              emitImmediateEventFields_ LoadHelpersContextTxOutRefNotFound [helperTxOutRef]
              -- Fail if the query is rejected.
              pure . SendMsgDone . Left $ LoadHelpersContextTxOutRefNotFoundError helperTxOutRef
          , recvMsgRollBackward =
              -- Start over if a rollback is encountered.
              \_ _ -> pure clientFindContract
          , recvMsgRollForward = \txs point _ -> case (point, Map.toList txs) of
              (Genesis, _) -> do
                emitImmediateEvent_ RollForwardToGenesis
                -- Unexpected roll forward to genesis.
                pure $ SendMsgDone $ Left RollForwardToGenesisError
              (At _, [(_, Transaction{txId, outputs})]) -> do
                emitImmediateEventFields_ TxOutRefConsumed [(helperTxOutRef, txId)]
                let isHelperScriptOutput (_, TransactionOutput{address, assets = Assets{tokens}}) =
                      address == helperAddress && AssetId helperPolicyId roleName `Map.member` unTokens tokens
                case filter isHelperScriptOutput $ zip (TxOutRef txId <$> [0 ..]) outputs of
                  [(helperTxOutRef', helperTransactionOutput')] ->
                    -- The head's helper script output has been consumed.
                    pure
                      . clientFollowHelper context
                      $ (roleName, state{helperTxOutRef = helperTxOutRef', helperTransactionOutput = helperTransactionOutput'})
                        : remainder
                  _ -> do
                    emitImmediateEventFields_ TxOutRefNotConsumed [helperTxOutRef]
                    -- There was not exactly one helper output.
                    pure $ clientFollowHelper context remainder
              (At _, _) -> do
                emitImmediateEventFields_ TxOutRefNotConsumed [helperTxOutRef]
                -- The helper output was not consumed exactly once.
                pure $ clientFollowHelper context remainder
          , recvMsgWait = do
              emitImmediateEventFields_ TxOutRefNotConsumed [helperTxOutRef]
              -- The tip of the head's helper script has been reached, so
              -- insert its state information into the context.
              pure
                . SendMsgCancel
                $ clientFollowHelper
                  context{helperScriptStates = Map.insert roleName state helperScriptStates}
                  remainder
          }
    clientFollowHelper helpersContext _ = SendMsgDone $ Right helpersContext

getHelperInfos
  :: (Ord k)
  => (HelperScriptInfo -> k)
  -> C.NetworkId
  -> MarloweScripts
  -> Map.Map k HelperScriptInfo
getHelperInfos f networkId MarloweScripts{helperScripts, helperScriptUTxOs} =
  let lookupHelper helper =
        do
          helperScript <- readMaybe helper
          helperScriptUTxO <- (helper, networkId) `Map.lookup` helperScriptUTxOs
          helperScriptHash <- helper `Map.lookup` helperScripts
          let helperAddress =
                fromCardanoAddressInEra C.BabbageEra $
                  C.AddressInEra (C.ShelleyAddressInEra C.ShelleyBasedEraBabbage) $
                    C.makeShelleyAddress
                      networkId
                      (C.PaymentCredentialByScript $ fromJust $ toCardanoScriptHash helperScriptHash)
                      C.NoStakeAddress
              info = HelperScriptInfo{..}
          pure (f info, info)
   in Map.fromList . mapMaybe lookupHelper $ Map.keys helperScripts
