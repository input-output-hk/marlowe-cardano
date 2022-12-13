{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}

module Language.Marlowe.Runtime.Transaction.Query
  where

import Cardano.Api (NetworkId)
import qualified Cardano.Api as C
import Data.Foldable (find)
import Data.List (scanl')
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Type.Equality (testEquality, type (:~:)(Refl))
import Data.Void (Void)
import Language.Marlowe.Protocol.Sync.Client
import Language.Marlowe.Runtime.Cardano.Api
import Language.Marlowe.Runtime.ChainSync.Api
  (Address, BlockHeader, Credential(..), GetUTxOsQuery(..), ScriptHash, TxOutRef, UTxOs(..), paymentCredential)
import Language.Marlowe.Runtime.Core.Api
import Language.Marlowe.Runtime.Core.ScriptRegistry (MarloweScripts(..), ReferenceScriptUtxo, getScripts)
import Language.Marlowe.Runtime.History.Api
import Language.Marlowe.Runtime.Transaction.Api
import Language.Marlowe.Runtime.Transaction.Constraints
import qualified Language.Marlowe.Runtime.Transaction.Constraints as Constraints
import Observe.Event (EventBackend, addField, withEvent)

data LoadWalletContextSelector f where
  LoadWalletContext :: LoadWalletContextSelector LoadWalletContextField

data LoadWalletContextField
  = ForAddresses (Set Address)
  | WalletContextLoaded WalletContext

data LoadMarloweContextSelector f where
  ContractNotFound :: LoadMarloweContextSelector Void
  ContractFound :: LoadMarloweContextSelector ContractFoundField
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
  :: C.NetworkId
  -> (forall a. MarloweSyncClient IO a -> IO a)
  -> LoadMarloweContext r
loadMarloweContext networkId runClient eventBackend desiredVersion contractId = runClient client
  where
    client = MarloweSyncClient . pure $ clientInit
    -- Start by following the contract
    clientInit = SendMsgFollowContract contractId clientFollow

    clientFollow = ClientStFollow
      -- If the contract isn't found, return an error
      { recvMsgContractNotFound = withEvent eventBackend ContractNotFound
          $ const
          $ pure
          $ Left LoadMarloweContextErrorNotFound
      -- Otherwise,
      , recvMsgContractFound = \blockHeader actualVersion CreateStep{..} -> withEvent eventBackend ContractFound \ev -> do
          addField ev $ ActualVersion actualVersion
          addField ev $ PayoutScriptHash payoutValidatorHash
          let TransactionScriptOutput scriptAddress _ utxo _ = createOutput
          addField ev $ MarloweScriptAddress scriptAddress
          addField ev $ ContractUTxO utxo
          -- Otherwise, check the desired version matches the actual one
          either (pure . SendMsgDone . Left) pure case testEquality desiredVersion actualVersion of
            Nothing -> pure
              $ SendMsgDone
              $ Left
              $ LoadMarloweContextErrorVersionMismatch
              $ SomeMarloweVersion actualVersion
            -- Build the initial context and use it as a seed for the loop.
            Just Refl -> case toCardanoScriptHash payoutValidatorHash of
              Nothing -> pure
                $ SendMsgDone
                $ Left LoadMarloweContextToCardanoError
              Just payoutScriptHash -> do
                let
                  payoutAddress = fromCardanoAddressInEra C.BabbageEra
                    $ C.AddressInEra (C.ShelleyAddressInEra C.ShelleyBasedEraBabbage)
                    $ C.makeShelleyAddress
                        networkId
                        (C.PaymentCredentialByScript payoutScriptHash)
                        C.NoStakeAddress
                  scripts = getScripts actualVersion
                marloweScriptHash <- maybe (Left $ InvalidScriptAddress scriptAddress) Right do
                  credential <- paymentCredential scriptAddress
                  case credential of
                    PaymentKeyCredential _ -> Nothing
                    ScriptCredential hash -> Just hash
                let matchesScriptHash MarloweScripts{..} = marloweScript == marloweScriptHash
                marloweScripts <- case find matchesScriptHash scripts of
                  Nothing -> Left $ UnknownMarloweScript marloweScriptHash
                  Just marloweScripts -> Right marloweScripts
                marloweScriptUTxO <- lookupMarloweScriptUtxo networkId marloweScripts
                payoutScriptUTxO <- lookupPayoutScriptUtxo networkId marloweScripts
                pure $ clientIdle actualVersion $ pure
                  ( blockHeader
                  , MarloweContext
                      { marloweAddress = scriptAddress
                      , payoutScriptHash = payoutValidatorHash
                      , marloweScriptHash
                      , payoutAddress
                      -- Get the script output of the create event.
                      , scriptOutput = Just createOutput
                      -- No payouts to start with
                      , payoutOutputs = mempty
                      , marloweScriptUTxO
                      , payoutScriptUTxO
                      }
                  )
        }

    -- Request the next steps in the contract
    clientIdle
      :: MarloweVersion v
      -> NonEmpty (BlockHeader, MarloweContext v)
      -> ClientStIdle v IO (Either LoadMarloweContextError (MarloweContext v))
    clientIdle version = SendMsgRequestNext . clientNext version

    clientNext
      :: MarloweVersion v
      -> NonEmpty (BlockHeader, MarloweContext v)
      -> ClientStNext v IO (Either LoadMarloweContextError (MarloweContext v))
    clientNext version contexts = ClientStNext
      -- If the creation event was rolled back, return an error
      { recvMsgRollBackCreation =
          (pure :: forall a. a -> IO a) $ Left LoadMarloweContextErrorNotFound
      -- Handle rollbacks
      , recvMsgRollBackward =
          pure . handleRollback version contexts
      -- Handle new steps
      , recvMsgRollForward = \blockHeader ->
          pure . clientIdle version . appendSteps contexts blockHeader
      -- If we are told to wait (because there are no more steps to send),
      -- we've reached the tip of the contract. Return the most recent
      -- accumulated context.
      , recvMsgWait = withEvent eventBackend (ContractTipFound version) \ev -> do
          let context = snd $ NE.head contexts
          addField ev context
          pure $ SendMsgCancel $ SendMsgDone $ Right context
      }

    handleRollback
      :: MarloweVersion v
      -> NonEmpty (BlockHeader, MarloweContext v)
      -> BlockHeader
      -> ClientStIdle v IO (Either LoadMarloweContextError (MarloweContext v))
    handleRollback version contexts@((blockHeader, _) :| contexts') rollbackBlock
      -- If the latest block is after the rollback block
      | blockHeader > rollbackBlock = case contexts' of
          -- And the previous contexts is non-empty, keep rolling back
          s : ss -> handleRollback version (s :| ss) rollbackBlock
          -- And the previous contexts is empty, return an error
          [] -> SendMsgDone $ Left LoadMarloweContextErrorNotFound
      -- Otherwise, resume from this block.
      | otherwise = clientIdle version contexts

    appendSteps contexts@((_, prevContext) :| _) blockHeader steps =
      prependList
        -- drop 1 because scanl' starts with the seed value (i.e. prevContext)
        ((blockHeader,) <$> drop 1 (scanl' applyStep prevContext steps))
        contexts

    prependList :: [a] -> NonEmpty a -> NonEmpty a
    prependList = \case
      [] -> id
      x : xs -> \(y :| ys) -> x :| xs <> (y : ys)

    -- Update the context with a new step
    applyStep context = \case
      -- For ApplyTransaction steps
      ApplyTransaction Transaction{output = TransactionOutput{..}} -> context
        -- Replace the scriptOutput with that of the new transaction
        { Constraints.scriptOutput = scriptOutput
        -- Add new payouts
        , Constraints.payoutOutputs = payoutOutputs context <> payouts
        }
      -- For RedeemPayout steps
      RedeemPayout RedeemStep{..} -> context
        -- Remove the payout that was redeemed
        { Constraints.payoutOutputs = Map.delete utxo $ payoutOutputs context
        }

lookupMarloweScriptUtxo :: NetworkId -> MarloweScripts -> Either LoadMarloweContextError ReferenceScriptUtxo
lookupMarloweScriptUtxo networkId MarloweScripts{..} =
  maybe (Left $ MarloweScriptNotPublished marloweScript) Right
    $ Map.lookup networkId marloweScriptUTxOs

lookupPayoutScriptUtxo :: NetworkId -> MarloweScripts -> Either LoadMarloweContextError ReferenceScriptUtxo
lookupPayoutScriptUtxo networkId MarloweScripts{..} =
  maybe (Left $ PayoutScriptNotPublished payoutScript) Right
    $ Map.lookup networkId payoutScriptUTxOs
