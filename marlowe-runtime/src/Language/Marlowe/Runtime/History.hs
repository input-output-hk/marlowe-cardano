{-# LANGUAGE BlockArguments             #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE EmptyDataDeriving          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE StrictData                 #-}
{-# LANGUAGE TupleSections              #-}
module Language.Marlowe.Runtime.History where

import Cardano.Api (AddressInEra (AddressInEra), AsType (..), AssetId (AdaAssetId, AssetId), AssetName (AssetName),
                    NetworkId, Quantity (Quantity), SerialiseAsRawBytes (deserialiseFromRawBytes), toAddressAny,
                    valueFromList)
import Cardano.Api.Shelley (toPlutusData)
import Control.Monad (forM)
import Data.Bifunctor (Bifunctor (first))
import Data.ByteString.Char8 (unpack)
import Data.List (find)
import Data.Maybe (maybeToList)
import Language.Marlowe (MarloweParams (..), Token (..), marloweParams)
import Language.Marlowe.Runtime.Chain.Types (MarloweAddress (..), MarloweBlockHeader (..), MarlowePolicyId (..),
                                             MarloweTx (..), MarloweTxIn (..), MarloweTxOut (..), TxOutRef (..),
                                             matchOutputRef)
import Language.Marlowe.Runtime.History.Types (Account (..), AppTxOutRef (..), Assets (..), Choice (..),
                                               ChoiceSelection (..), ContinuationHash (..), ContractContinuation (..),
                                               ContractCreationTxOut (..), ContractId (..), Datum (..), Event (..),
                                               HistoryEvent (..), Input (..), Participant (..), ValidatorHash (..))
import Language.Marlowe.Scripts (smallUntypedValidator)
import qualified Language.Marlowe.Semantics.Types as Semantics
import Ledger (MintingPolicyHash (..))
import Ledger.Tx.CardanoAPI (fromCardanoPolicyId, toCardanoAddress, toCardanoScriptHash)
import Ledger.Typed.Scripts (validatorAddress)
import Ledger.Value (CurrencySymbol (..), TokenName (..), toString)
import Plutus.V1.Ledger.Api (fromBuiltin, fromData)

extractCreations :: NetworkId -> MarloweBlockHeader -> MarloweTx -> [Either ([TxOutRef], String) ContractCreationTxOut]
extractCreations networkId header MarloweTx{..} = do
  policyId <- marloweTx_policies
  let MintingPolicyHash policyHash = fromCardanoPolicyId $ unMarlowePolicyId policyId
  let params@MarloweParams{..} = marloweParams $ CurrencySymbol policyHash
  let applicationValidatorPlutusAddress = validatorAddress $ smallUntypedValidator params
  maybeToList $ sequence do
    AddressInEra _ applicationValidatorCardanoAddress <- first (([],) . show) $ toCardanoAddress networkId applicationValidatorPlutusAddress
    let applicationValidatorAddress = MarloweAddress $ toAddressAny applicationValidatorCardanoAddress
    payoutValidatorHash <- first (([],) . show) $ toCardanoScriptHash rolePayoutValidatorHash
    let contractId = ContractId policyId $ ValidatorHash payoutValidatorHash
    exclusive do
      matchingTxOut <- filter (wasSentToAddress applicationValidatorAddress) marloweTx_outputs
      datum <- maybeToList $ hush $ extractDatum matchingTxOut
      pure $ ContractCreationTxOut contractId datum matchingTxOut header
    where
      wasSentToAddress address MarloweTxOut{..} = address == marloweTxOut_address
      exclusive []    = Right Nothing
      exclusive [out] = Right $ Just out
      exclusive outs  = Left (outToTxRef <$> outs, "multiple candidate contract creation TxOs found for contract ")
      outToTxRef = marloweTxOut_txOutRef . txOut

extractDatum :: MarloweTxOut -> Either String Datum
extractDatum matchingTxOut = do
  scriptData <- case marloweTxOut_datum matchingTxOut of
    Nothing -> Left "No datum at tx out"
    Just d  -> Right d
  datum <- case fromData $ toPlutusData scriptData of
    Nothing -> Left "Invalid datum at tx out"
    Just d  -> Right d
  pure $ Datum datum

creationToEvent :: ContractCreationTxOut -> Event
creationToEvent creation@ContractCreationTxOut{..} =
  let
    blockHeader = header
    (TxOutRef txId _) = marloweTxOut_txOutRef txOut
    historyEvent = ContractWasCreated creation
  in
    Event{..}

extractEvents
  :: MarloweAddress -- ^ Marlowe validator address
  -> ContractId -- ^ The marlowe params
  -> AppTxOutRef -- ^ The UTxO from the previous transaction
  -> MarloweBlockHeader -- ^ The block header for the current tx
  -> MarloweTx -- ^ The current tx
  -> Either String (Maybe AppTxOutRef, [Event])
extractEvents applicationValidatorAddress contractId prevUtxo header MarloweTx{..} = do
  consumedInput <- case find (matchOutputRef (txOutRef prevUtxo)) marloweTx_inputs of
    Nothing    -> Left "The next transaction did not contain any extracable events"
    Just input -> Right input
  let nextUtxo = find ((applicationValidatorAddress ==) . marloweTxOut_address) marloweTx_outputs
  appTxOut <- forM nextUtxo \txOut -> AppTxOutRef (marloweTxOut_txOutRef txOut) <$> extractDatum txOut
  inputs <- extractInputs consumedInput
  let historyEvents = [InputsWereApplied{..}]
  pure (appTxOut, Event contractId header marloweTx_id <$> historyEvents)


extractInputs :: MarloweTxIn -> Either String [Input]
extractInputs (MarloweTxIn _ _ Nothing) = Left "No redeemer found"
extractInputs (MarloweTxIn _ _ (Just redeemer)) = case fromData redeemer of
  Nothing     -> Left "Unable to parse redeemer"
  Just inputs -> traverse fromSemanticInput inputs

fromSemanticInput :: Semantics.Input -> Either String Input
fromSemanticInput (Semantics.NormalInput content) = fromSemanticInputContent content Nothing
fromSemanticInput (Semantics.MerkleizedInput content hashBytes continuation) = do
  continuationHash <- case deserialiseFromRawBytes (AsHash AsScriptData) $ fromBuiltin hashBytes of
    Nothing   -> Left "Invlid continuation hash bytes"
    Just hash -> Right $ ContinuationHash hash
  fromSemanticInputContent content $ Just $ ContractContinuation{..}

fromSemanticInputContent :: Semantics.InputContent -> Maybe ContractContinuation -> Either String Input
fromSemanticInputContent (Semantics.IDeposit accountId party token quantity) continuation = do
  let Token (CurrencySymbol currencySymbol) (TokenName tokenName) = token
  assetId <- case fromBuiltin currencySymbol of
    "" -> Right AdaAssetId
    symbol -> case deserialiseFromRawBytes AsPolicyId symbol of
      Nothing       -> Left "Invalid policyId bytes"
      Just policyId -> Right $ AssetId policyId $ AssetName $ fromBuiltin tokenName
  intoAccount <- case accountId of
    Semantics.Role role -> Right $ RoleAccount $ toString role
    Semantics.PK _      -> Left "only roles and addresses supported"
  fromParty <- case party of
    Semantics.Role role -> Right $ RoleParticipant $ toString role
    Semantics.PK _      -> Left "only roles and addresses supported"
  let assets = Assets  $ valueFromList [(assetId, Quantity quantity)]
  pure AssetsWereDeposited{..}
fromSemanticInputContent (Semantics.IChoice (Semantics.ChoiceId name party) num) continuation = do
  choice <- Choice (unpack $ fromBuiltin name) <$> case party of
    Semantics.Role role -> Right $ RoleParticipant $ toString role
    Semantics.PK _      -> Left "only roles and addresses supported"
  let selection = ChoiceSelection num
  pure ChoiceWasMade{..}
fromSemanticInputContent Semantics.INotify continuation =
  pure $ NotifyWasMade continuation

-- extractHistoryEvent
--   :: MarloweAddress -> ContractId -> MarloweTxOut -> Either (TxOutRef, String) (Maybe HistoryEvent)
-- extractHistoryEvent applicationValidatorAddress contractId MarloweTxOut{..} =


hush :: Either b a -> Maybe a
hush = either (const Nothing) Just
