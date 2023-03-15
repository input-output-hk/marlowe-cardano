module Language.Marlowe.Runtime.Web.Common
  where

import Cardano.Api
  ( AsType(..)
  , ShelleyWitnessSigningKey(..)
  , TextEnvelope(..)
  , TextEnvelopeType(..)
  , deserialiseFromTextEnvelope
  , serialiseToTextEnvelope
  , signShelleyTransaction
  )
import Cardano.Api.SerialiseTextEnvelope (TextEnvelopeDescr(..))
import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class (MonadIO(liftIO))

import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Language.Marlowe as V1
import Language.Marlowe.Core.V1.Semantics.Types
  (ChoiceId(ChoiceId), Input(NormalInput), InputContent(IChoice, IDeposit, INotify))
import Language.Marlowe.Runtime.Integration.Common
import Language.Marlowe.Runtime.Transaction.Api (WalletAddresses(..))
import qualified Language.Marlowe.Runtime.Web as Web
import Language.Marlowe.Runtime.Web.Client
  ( getContract
  , getTransaction
  , postContract
  , postTransaction
  , postWithdrawal
  , putContract
  , putTransaction
  , putWithdrawal
  )
import Language.Marlowe.Runtime.Web.Server.DTO (ToDTO(toDTO))
import qualified Plutus.V2.Ledger.Api as PV2
import Servant.Client (ClientM)

createCloseContract :: Wallet -> ClientM Web.TxOutRef
createCloseContract Wallet{..} = do
  let WalletAddresses{..} = addresses
  let webChangeAddress = toDTO changeAddress
  let webExtraAddresses = Set.map toDTO extraAddresses
  let webCollataralUtxos = Set.map toDTO collateralUtxos

  Web.CreateTxBody{txBody = createTxBody, ..} <- postContract
    webChangeAddress
    (Just webExtraAddresses)
    (Just webCollataralUtxos)
    Web.PostContractsRequest
      { metadata = mempty
      , version = Web.V1
      , roles = Nothing
      , contract = V1.Close
      , minUTxODeposit = 2_000_000
      , tags = mempty
      }

  createTx <- liftIO $ signShelleyTransaction' createTxBody signingKeys
  putContract contractId createTx
  _ <- waitUntilConfirmed (\Web.ContractState{status} -> status) $ getContract contractId
  pure contractId

applyCloseTransaction :: Wallet -> Web.TxOutRef -> ClientM Web.TxId
applyCloseTransaction  Wallet{..} contractId = do
  let WalletAddresses{..} = addresses
  let webChangeAddress = toDTO changeAddress
  let webExtraAddresses = Set.map toDTO extraAddresses
  let webCollataralUtxos = Set.map toDTO collateralUtxos
  Web.ApplyInputsTxBody{transactionId, txBody = applyTxBody} <- postTransaction
    webChangeAddress
    (Just webExtraAddresses)
    (Just webCollataralUtxos)
    contractId
    Web.PostTransactionsRequest
      { version = Web.V1
      , metadata = mempty
      , invalidBefore = Nothing
      , invalidHereafter = Nothing
      , inputs = []
      , tags = mempty
      }

  applyTx <- liftIO $ signShelleyTransaction' applyTxBody signingKeys

  putTransaction contractId transactionId applyTx

  _ <- waitUntilConfirmed (\Web.Tx{status} -> status) $ getTransaction contractId transactionId
  pure transactionId


signShelleyTransaction' :: Web.TextEnvelope -> [ShelleyWitnessSigningKey] -> IO Web.TextEnvelope
signShelleyTransaction' Web.TextEnvelope{..} wits = do
  let te = TextEnvelope { teType = TextEnvelopeType (T.unpack teType), teDescription = TextEnvelopeDescr (T.unpack teDescription), teRawCBOR = Web.unBase16 teCborHex }
  txBody <- case deserialiseFromTextEnvelope (AsTxBody AsBabbage) te of
    Left err -> fail $ show err
    Right a -> pure a
  pure case serialiseToTextEnvelope Nothing $ signShelleyTransaction txBody wits of
    TextEnvelope (TextEnvelopeType ty) _ bytes -> Web.TextEnvelope (T.pack ty) "" $ Web.Base16 bytes

waitUntilConfirmed :: MonadIO m => (a -> Web.TxStatus) -> m a -> m a
waitUntilConfirmed getStatus getResource = do
  resource <- getResource
  case getStatus resource of
    Web.Confirmed -> pure resource
    _ -> do
      liftIO $ threadDelay 1000
      waitUntilConfirmed getStatus getResource

submitContract
  :: Wallet
  -> Web.TxOutRef
  -> Web.TextEnvelope
  -> ClientM Web.BlockHeader
submitContract Wallet{..} contractId txBody = do
  signedCreateTx <- liftIO $ signShelleyTransaction' txBody signingKeys
  putContract contractId signedCreateTx
  Web.ContractState{block} <- waitUntilConfirmed (\Web.ContractState{status} -> status) $ getContract contractId
  liftIO $ expectJust "Expected a block header" block

submitTransaction
  :: Wallet
  -> Web.TxOutRef
  -> Web.TxId
  -> Web.TextEnvelope
  -> ClientM Web.BlockHeader
submitTransaction Wallet{..} contractId transactionId txBody = do
  signedTx <- liftIO $ signShelleyTransaction' txBody signingKeys
  putTransaction contractId transactionId signedTx
  Web.ContractState{block} <- waitUntilConfirmed (\Web.ContractState{status} -> status) $ getContract contractId
  liftIO $ expectJust "Expected a block header" block

submitWithdrawal
  :: Wallet
  -> Web.TxOutRef
  -> Web.TxId
  -> Web.TextEnvelope
  -> ClientM Web.BlockHeader
submitWithdrawal Wallet{..} contractId transactionId withdrawTxBody  = do
  signedWithdrawalTx <- liftIO $ signShelleyTransaction' withdrawTxBody signingKeys
  putWithdrawal transactionId signedWithdrawalTx
  Web.Tx{block} <- waitUntilConfirmed (\Web.Tx{status} -> status) $ getTransaction contractId transactionId
  liftIO $ expectJust "Expected a block header" block

deposit
  :: Wallet
  -> Web.TxOutRef
  -> V1.Party
  -> V1.Party
  -> V1.Token
  -> Integer
  -> ClientM Web.BlockHeader
deposit Wallet{..} contractId intoAccount fromParty ofToken quantity = do
  let WalletAddresses{..} = addresses
  let webChangeAddress = toDTO changeAddress
  let webExtraAddresses = Set.map toDTO extraAddresses
  let webCollataralUtxos = Set.map toDTO collateralUtxos


  Web.ApplyInputsTxBody{transactionId, txBody = applyTxBody} <- postTransaction
    webChangeAddress
    (Just webExtraAddresses)
    (Just webCollataralUtxos)
    contractId
    Web.PostTransactionsRequest
      { version = Web.V1
      , metadata = mempty
      , invalidBefore = Nothing
      , invalidHereafter = Nothing
      , inputs = [NormalInput $ IDeposit intoAccount fromParty ofToken quantity]
      , tags = mempty
      }

  applyTx <- liftIO $ signShelleyTransaction' applyTxBody signingKeys

  putTransaction contractId transactionId applyTx

  Web.ContractState{block} <- waitUntilConfirmed (\Web.ContractState{status} -> status) $ getContract contractId
  liftIO $ expectJust "Expected a block header" block

choose
  :: Wallet
  -> Web.TxOutRef
  -> PV2.BuiltinByteString
  -> V1.Party
  -> Integer
  -> ClientM Web.BlockHeader
choose Wallet{..} contractId choice party chosenNum = do
  let WalletAddresses{..} = addresses
  let webChangeAddress = toDTO changeAddress
  let webExtraAddresses = Set.map toDTO extraAddresses
  let webCollataralUtxos = Set.map toDTO collateralUtxos


  Web.ApplyInputsTxBody{transactionId, txBody = applyTxBody} <- postTransaction
    webChangeAddress
    (Just webExtraAddresses)
    (Just webCollataralUtxos)
    contractId
    Web.PostTransactionsRequest
      { version = Web.V1
      , metadata = mempty
      , invalidBefore = Nothing
      , invalidHereafter = Nothing
      , inputs = [NormalInput $ IChoice (ChoiceId choice party) chosenNum]
      , tags = mempty
      }

  applyTx <- liftIO $ signShelleyTransaction' applyTxBody signingKeys

  putTransaction contractId transactionId applyTx

  Web.ContractState{block} <- waitUntilConfirmed (\Web.ContractState{status} -> status) $ getContract contractId
  liftIO $ expectJust "Expected a block header" block

notify
  :: Wallet
  -> Web.TxOutRef
  -> ClientM Web.BlockHeader
notify Wallet{..} contractId = do
  let WalletAddresses{..} = addresses
  let webChangeAddress = toDTO changeAddress
  let webExtraAddresses = Set.map toDTO extraAddresses
  let webCollataralUtxos = Set.map toDTO collateralUtxos


  Web.ApplyInputsTxBody{transactionId, txBody = applyTxBody} <- postTransaction
    webChangeAddress
    (Just webExtraAddresses)
    (Just webCollataralUtxos)
    contractId
    Web.PostTransactionsRequest
      { version = Web.V1
      , metadata = mempty
      , invalidBefore = Nothing
      , invalidHereafter = Nothing
      , inputs = [NormalInput INotify]
      , tags = mempty
      }

  applyTx <- liftIO $ signShelleyTransaction' applyTxBody signingKeys

  putTransaction contractId transactionId applyTx

  Web.ContractState{block} <- waitUntilConfirmed (\Web.ContractState{status} -> status) $ getContract contractId
  liftIO $ expectJust "Expected a block header" block

withdraw
  :: Wallet
  -> Web.TxOutRef
  -> T.Text
  -> ClientM Web.BlockHeader
withdraw Wallet{..} contractId role = do
  let WalletAddresses{..} = addresses
  let webChangeAddress = toDTO changeAddress
  let webExtraAddresses = Set.map toDTO extraAddresses
  let webCollataralUtxos = Set.map toDTO collateralUtxos

  Web.WithdrawTxBody{withdrawalId, txBody = withdrawTxBody} <- postWithdrawal
    webChangeAddress
    (Just webExtraAddresses)
    (Just webCollataralUtxos)
    Web.PostWithdrawalsRequest
      { role
      , contractId
      }

  withdrawTx <- liftIO $ signShelleyTransaction' withdrawTxBody signingKeys

  putWithdrawal withdrawalId withdrawTx

  Web.ContractState{block} <- waitUntilConfirmed (\Web.ContractState{status} -> status) $ getContract contractId

  liftIO $ expectJust "Expected a block header" block

