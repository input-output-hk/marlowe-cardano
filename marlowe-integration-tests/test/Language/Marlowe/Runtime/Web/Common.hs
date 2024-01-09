module Language.Marlowe.Runtime.Web.Common (
  applyCloseTransaction,
  applyInputs,
  choose,
  createCloseContract,
  deposit,
  notify,
  signShelleyTransaction',
  submitContract,
  submitTransaction,
  submitWithdrawal,
  waitUntilConfirmed,
  withdraw,
) where

import Cardano.Api (
  BabbageEra,
  ShelleyWitnessSigningKey (..),
  Tx,
  getTxBody,
  getTxWitnesses,
  signShelleyTransaction,
 )
import Cardano.Api.Shelley (KeyWitness (..))
import Cardano.Ledger.Alonzo.TxWits (Redeemers (..))
import Cardano.Ledger.Babbage.TxWits (AlonzoTxWits (AlonzoTxWits))
import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Aeson.Text (encodeToLazyText)
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text.Lazy as TL
import qualified Language.Marlowe as V1
import Language.Marlowe.Core.V1.Semantics.Types (
  ChoiceId (ChoiceId),
  Input (NormalInput),
  InputContent (IChoice, IDeposit, INotify),
 )
import Language.Marlowe.Runtime.Integration.Common hiding (choose, deposit, notify, withdraw)
import Language.Marlowe.Runtime.Transaction.Api (WalletAddresses (..))
import Language.Marlowe.Runtime.Web (ContractOrSourceId (..))
import qualified Language.Marlowe.Runtime.Web as Web
import Language.Marlowe.Runtime.Web.Client (
  getContract,
  getTransaction,
  getWithdrawal,
  postContract,
  postTransaction,
  postWithdrawal,
  putContract,
  putTransaction,
  putWithdrawal,
 )
import Language.Marlowe.Runtime.Web.Server.DTO (FromDTO (..), ToDTO (toDTO))
import Language.Marlowe.Runtime.Web.Server.Util (TxWitnessSet (TxWitnessSet))
import qualified PlutusLedgerApi.V2 as PV2
import Servant.Client.Streaming (ClientM)

createCloseContract :: Wallet -> ClientM Web.TxOutRef
createCloseContract Wallet{..} = do
  let WalletAddresses{..} = addresses
  let webChangeAddress = toDTO changeAddress
  let webExtraAddresses = Set.map toDTO extraAddresses
  let webCollateralUtxos = Set.map toDTO collateralUtxos

  Web.CreateTxEnvelope{tx, ..} <-
    postContract
      Nothing
      webChangeAddress
      (Just webExtraAddresses)
      (Just webCollateralUtxos)
      Web.PostContractsRequest
        { metadata = mempty
        , version = Web.V1
        , roles = Nothing
        , threadTokenName = Nothing
        , contract = ContractOrSourceId $ Left V1.Close
        , minUTxODeposit = Nothing
        , tags = mempty
        }

  createTx <- liftIO $ signShelleyTransaction' tx signingKeys
  putContract contractId createTx
  _ <- waitUntilConfirmed (\Web.ContractState{status} -> status) $ getContract contractId
  pure contractId

applyCloseTransaction :: Wallet -> Web.TxOutRef -> ClientM Web.TxId
applyCloseTransaction Wallet{..} contractId = do
  let WalletAddresses{..} = addresses
  let webChangeAddress = toDTO changeAddress
  let webExtraAddresses = Set.map toDTO extraAddresses
  let webCollateralUtxos = Set.map toDTO collateralUtxos
  Web.ApplyInputsTxEnvelope{transactionId, tx} <-
    postTransaction
      webChangeAddress
      (Just webExtraAddresses)
      (Just webCollateralUtxos)
      contractId
      Web.PostTransactionsRequest
        { version = Web.V1
        , metadata = mempty
        , invalidBefore = Nothing
        , invalidHereafter = Nothing
        , inputs = []
        , tags = mempty
        }

  applyTx <- liftIO $ signShelleyTransaction' tx signingKeys

  putTransaction contractId transactionId applyTx

  _ <- waitUntilConfirmed (\Web.Tx{status} -> status) $ getTransaction contractId transactionId
  pure transactionId

submitContract
  :: Wallet
  -> Web.CreateTxEnvelope
  -> ClientM Web.BlockHeader
submitContract Wallet{..} Web.CreateTxEnvelope{contractId, tx} = do
  signedCreateTx <- liftIO $ signShelleyTransaction' tx signingKeys
  liftIO $ putStrLn $ TL.unpack $ encodeToLazyText signedCreateTx
  putContract contractId signedCreateTx
  Web.ContractState{block} <- waitUntilConfirmed (\Web.ContractState{status} -> status) $ getContract contractId
  liftIO $ expectJust "Expected block header" block

submitTransaction
  :: Wallet
  -> Web.ApplyInputsTxEnvelope
  -> ClientM Web.BlockHeader
submitTransaction Wallet{..} Web.ApplyInputsTxEnvelope{contractId, transactionId, tx} = do
  signedTx <- liftIO $ signShelleyTransaction' tx signingKeys
  putTransaction contractId transactionId signedTx
  Web.Tx{block} <- waitUntilConfirmed (\Web.Tx{status} -> status) $ getTransaction contractId transactionId
  liftIO $ expectJust "Expected a block header" block

submitWithdrawal
  :: Wallet
  -> Web.WithdrawTxEnvelope
  -> ClientM Web.BlockHeader
submitWithdrawal Wallet{..} Web.WithdrawTxEnvelope{withdrawalId, tx} = do
  signedWithdrawalTx <- liftIO $ signShelleyTransaction' tx signingKeys
  putWithdrawal withdrawalId signedWithdrawalTx
  Web.Withdrawal{block} <- waitUntilConfirmed (\Web.Withdrawal{status} -> status) $ getWithdrawal withdrawalId
  liftIO $ expectJust "Expected a block header" block

deposit
  :: Wallet
  -> Web.TxOutRef
  -> V1.Party
  -> V1.Party
  -> V1.Token
  -> Integer
  -> ClientM Web.ApplyInputsTxEnvelope
deposit wallet contractId intoAccount fromParty ofToken quantity =
  applyInputs wallet contractId [NormalInput $ IDeposit intoAccount fromParty ofToken quantity]

choose
  :: Wallet
  -> Web.TxOutRef
  -> PV2.BuiltinByteString
  -> V1.Party
  -> Integer
  -> ClientM Web.ApplyInputsTxEnvelope
choose wallet contractId choice party chosenNum =
  applyInputs wallet contractId [NormalInput $ IChoice (ChoiceId choice party) chosenNum]

notify
  :: Wallet
  -> Web.TxOutRef
  -> ClientM Web.ApplyInputsTxEnvelope
notify wallet contractId = applyInputs wallet contractId [NormalInput INotify]

withdraw
  :: Wallet
  -> Set Web.TxOutRef
  -> ClientM Web.WithdrawTxEnvelope
withdraw Wallet{..} payouts = do
  let WalletAddresses{..} = addresses
  let webChangeAddress = toDTO changeAddress
  let webExtraAddresses = Set.map toDTO extraAddresses
  let webCollateralUtxos = Set.map toDTO collateralUtxos

  postWithdrawal
    webChangeAddress
    (Just webExtraAddresses)
    (Just webCollateralUtxos)
    Web.PostWithdrawalsRequest
      { payouts
      }
applyInputs
  :: Wallet
  -> Web.TxOutRef
  -> [V1.Input]
  -> ClientM Web.ApplyInputsTxEnvelope
applyInputs Wallet{..} contractId inputs = do
  let WalletAddresses{..} = addresses
  let webChangeAddress = toDTO changeAddress
  let webExtraAddresses = Set.map toDTO extraAddresses
  let webCollateralUtxos = Set.map toDTO collateralUtxos

  postTransaction
    webChangeAddress
    (Just webExtraAddresses)
    (Just webCollateralUtxos)
    contractId
    Web.PostTransactionsRequest
      { version = Web.V1
      , metadata = mempty
      , invalidBefore = Nothing
      , invalidHereafter = Nothing
      , inputs
      , tags = mempty
      }

signShelleyTransaction' :: Web.UnwitnessedTx -> [ShelleyWitnessSigningKey] -> IO Web.TxWitness
signShelleyTransaction' txEnvelope wits = do
  tx :: Tx BabbageEra <- expectJust "Failed to deserialise tx" $ fromDTO txEnvelope
  let keyWitness = head $ getTxWitnesses $ signShelleyTransaction (getTxBody tx) wits
  let vKeys = case keyWitness of
        ShelleyBootstrapWitness{} -> mempty
        ShelleyKeyWitness _ key -> Set.singleton key
  pure
    . toDTO
    . TxWitnessSet @BabbageEra
    $ AlonzoTxWits vKeys mempty mempty mempty
    $ Redeemers mempty

waitUntilConfirmed :: (MonadIO m) => (a -> Web.TxStatus) -> m a -> m a
waitUntilConfirmed getStatus getResource = do
  resource <- getResource
  case getStatus resource of
    Web.Confirmed -> pure resource
    _ -> do
      liftIO $ threadDelay 1000
      waitUntilConfirmed getStatus getResource
