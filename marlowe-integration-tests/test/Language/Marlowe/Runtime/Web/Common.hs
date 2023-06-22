module Language.Marlowe.Runtime.Web.Common
  ( applyCloseTransaction
  , applyInputs
  , choose
  , createCloseContract
  , deposit
  , notify
  , signShelleyTransaction'
  , submitContract
  , submitTransaction
  , submitWithdrawal
  , waitUntilConfirmed
  , withdraw
  ) where


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
import Language.Marlowe.Runtime.Integration.Common hiding (choose, deposit, notify, withdraw)
import Language.Marlowe.Runtime.Transaction.Api (WalletAddresses(..))
import qualified Language.Marlowe.Runtime.Web as Web
import Language.Marlowe.Runtime.Web.Client
  ( getContract
  , getTransaction
  , getWithdrawal
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

  Web.CreateTxEnvelope{txEnvelope, ..} <- postContract
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

  createTx <- liftIO $ signShelleyTransaction' txEnvelope signingKeys
  putContract contractId createTx
  _ <- waitUntilConfirmed (\Web.ContractState{status} -> status) $ getContract contractId
  pure contractId

applyCloseTransaction :: Wallet -> Web.TxOutRef -> ClientM Web.TxId
applyCloseTransaction  Wallet{..} contractId = do
  let WalletAddresses{..} = addresses
  let webChangeAddress = toDTO changeAddress
  let webExtraAddresses = Set.map toDTO extraAddresses
  let webCollataralUtxos = Set.map toDTO collateralUtxos
  Web.ApplyInputsTxEnvelope{transactionId, txEnvelope} <- postTransaction
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

  applyTx <- liftIO $ signShelleyTransaction' txEnvelope signingKeys

  putTransaction contractId transactionId applyTx

  _ <- waitUntilConfirmed (\Web.Tx{status} -> status) $ getTransaction contractId transactionId
  pure transactionId

submitContract
  :: Wallet
  -> Web.CreateTxEnvelope Web.CardanoTxBody
  -> ClientM Web.BlockHeader
submitContract Wallet{..} Web.CreateTxEnvelope{contractId, txEnvelope}= do
  signedCreateTx <- liftIO $ signShelleyTransaction' txEnvelope signingKeys
  putContract contractId signedCreateTx
  Web.ContractState{block} <- waitUntilConfirmed (\Web.ContractState{status} -> status) $ getContract contractId
  liftIO $ expectJust "Expected block header" block

submitTransaction
  :: Wallet
  -> Web.ApplyInputsTxEnvelope Web.CardanoTxBody
  -> ClientM Web.BlockHeader
submitTransaction Wallet{..} Web.ApplyInputsTxEnvelope{contractId, transactionId, txEnvelope} = do
  signedTx <- liftIO $ signShelleyTransaction' txEnvelope signingKeys
  putTransaction contractId transactionId signedTx
  Web.Tx{block} <- waitUntilConfirmed (\Web.Tx{status} -> status) $ getTransaction contractId transactionId
  liftIO $ expectJust "Expected a block header" block

submitWithdrawal
  :: Wallet
  -> Web.WithdrawTxEnvelope Web.CardanoTxBody
  -> ClientM Web.BlockHeader
submitWithdrawal Wallet{..} Web.WithdrawTxEnvelope{withdrawalId, txEnvelope}  = do
  signedWithdrawalTx <- liftIO $ signShelleyTransaction' txEnvelope signingKeys
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
  -> ClientM (Web.ApplyInputsTxEnvelope Web.CardanoTxBody)
deposit wallet contractId intoAccount fromParty ofToken quantity =
  applyInputs wallet contractId [NormalInput $ IDeposit intoAccount fromParty ofToken quantity]

choose
  :: Wallet
  -> Web.TxOutRef
  -> PV2.BuiltinByteString
  -> V1.Party
  -> Integer
  -> ClientM (Web.ApplyInputsTxEnvelope Web.CardanoTxBody)
choose wallet contractId choice party chosenNum =
  applyInputs wallet contractId [NormalInput $ IChoice (ChoiceId choice party) chosenNum]

notify
  :: Wallet
  -> Web.TxOutRef
  -> ClientM (Web.ApplyInputsTxEnvelope Web.CardanoTxBody)
notify wallet contractId = applyInputs wallet contractId [NormalInput INotify]

withdraw
  :: Wallet
  -> Web.TxOutRef
  -> T.Text
  -> ClientM (Web.WithdrawTxEnvelope Web.CardanoTxBody)
withdraw Wallet{..} contractId role = do
  let WalletAddresses{..} = addresses
  let webChangeAddress = toDTO changeAddress
  let webExtraAddresses = Set.map toDTO extraAddresses
  let webCollataralUtxos = Set.map toDTO collateralUtxos

  postWithdrawal
    webChangeAddress
    (Just webExtraAddresses)
    (Just webCollataralUtxos)
    Web.PostWithdrawalsRequest
      { role
      , contractId
      }
applyInputs
  :: Wallet
  -> Web.TxOutRef
  -> [V1.Input]
  -> ClientM (Web.ApplyInputsTxEnvelope Web.CardanoTxBody)
applyInputs Wallet{..} contractId inputs = do
  let WalletAddresses{..} = addresses
  let webChangeAddress = toDTO changeAddress
  let webExtraAddresses = Set.map toDTO extraAddresses
  let webCollataralUtxos = Set.map toDTO collateralUtxos

  postTransaction
    webChangeAddress
    (Just webExtraAddresses)
    (Just webCollataralUtxos)
    contractId
    Web.PostTransactionsRequest
      { version = Web.V1
      , metadata = mempty
      , invalidBefore = Nothing
      , invalidHereafter = Nothing
      , inputs
      , tags = mempty
      }

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
