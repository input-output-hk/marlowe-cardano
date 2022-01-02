-----------------------------------------------------------------------------
--
-- Module      :  $Headers
-- License     :  Apache 2.0
--
-- Stability   :  Experimental
-- Portability :  Portable
--
-- | Run Marlowe contracts.
--
-----------------------------------------------------------------------------


{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}


module Language.Marlowe.CLI.Run (
-- * Computation
  initializeTransaction
, initializeTransactionImpl
, prepareTransaction
, makeMarlowe
, runTransaction
-- * Roles
, withdrawFunds
-- * Input
, makeDeposit
, makeChoice
, makeNotification
) where


import           Cardano.Api                      (AddressAny, AddressInEra (..), AlonzoEra, CardanoMode,
                                                   LocalNodeConnectInfo (localNodeNetworkId),
                                                   MultiAssetSupportedInEra (MultiAssetInAlonzoEra), NetworkId,
                                                   QueryInShelleyBasedEra (QueryProtocolParameters, QueryUTxO),
                                                   QueryUTxOFilter (QueryUTxOByAddress), Script (..),
                                                   ScriptDataSupportedInEra (..),
                                                   ShelleyBasedEra (ShelleyBasedEraAlonzo), SlotNo (..),
                                                   StakeAddressReference (..), TxId, TxIn, TxOut (..), TxOutDatum (..),
                                                   TxOutValue (..), UTxO (..), calculateMinimumUTxO, getTxId,
                                                   lovelaceToValue, selectLovelace, toAddressAny, txOutValueToValue,
                                                   writeFileTextEnvelope)
import           Cardano.Api.Shelley              (ProtocolParameters, fromPlutusData)
import           Control.Monad                    (forM_, guard, unless, when)
import           Control.Monad.Except             (MonadError, MonadIO, liftIO, throwError)
import           Data.Bifunctor                   (bimap)
import           Data.Function                    (on)
import           Data.List                        (groupBy)
import           Data.Maybe                       (catMaybes, fromMaybe)
import           Language.Marlowe.CLI.Export      (buildDatum, buildRedeemer, buildRoleDatum, buildRoleRedeemer,
                                                   buildRoleValidator, buildValidator)
import           Language.Marlowe.CLI.IO          (decodeFileStrict, liftCli, liftCliIO, maybeWriteJson, readSigningKey)
import           Language.Marlowe.CLI.Orphans     ()
import           Language.Marlowe.CLI.Transaction (buildBody, buildPayFromScript, buildPayToScript, hashSigningKey,
                                                   queryAlonzo, submitBody)
import           Language.Marlowe.CLI.Types       (CliError (..), DatumInfo (..), MarloweTransaction (..),
                                                   RedeemerInfo (..), ValidatorInfo (..))
import           Language.Marlowe.Semantics       (MarloweParams (rolesCurrency), Payment (..), TransactionInput (..),
                                                   TransactionOutput (..), TransactionWarning, computeTransaction)
import           Language.Marlowe.SemanticsTypes  (AccountId, ChoiceId (..), ChoiceName, ChosenNum, Contract,
                                                   Input (..), InputContent (..), Party (..), Payee (..),
                                                   State (accounts), Token (..))
import           Ledger.Tx.CardanoAPI             (toCardanoAddress, toCardanoScriptDataHash, toCardanoValue)
import           Plutus.V1.Ledger.Ada             (adaSymbol, adaToken, fromValue, getAda)
import           Plutus.V1.Ledger.Api             (Address (..), CostModelParams, Credential (..), Datum, POSIXTime,
                                                   TokenName, toData)
import           Plutus.V1.Ledger.Slot            (Slot (..))
import           Plutus.V1.Ledger.Value           (AssetClass (..), Value (..), assetClassValue)
import           Prettyprinter.Extras             (Pretty (..))
import           System.IO                        (hPutStrLn, stderr)

import qualified Cardano.Api                      as Api (Value)
import qualified Data.Map.Strict                  as M (toList)
import qualified Data.Set                         as S (singleton)
import qualified PlutusTx.AssocMap                as AM (toList)


-- | Serialise a deposit input to a file.
makeDeposit :: MonadIO m
            => AccountId       -- ^ The account for the deposit.
            -> Party           -- ^ The party making the deposit.
            -> Maybe Token     -- ^ The token being deposited.
            -> Integer         -- ^ The amount of the token deposited.
            -> Maybe FilePath  -- ^ The output JSON file representing the input.
            -> m ()            -- ^ Action to write the input to the file.
makeDeposit accountId party token amount outputFile =
  maybeWriteJson outputFile
    . NormalInput
    $ IDeposit accountId party (fromMaybe (Token adaSymbol adaToken) token) amount


-- | Serialise a choice input to a file.
makeChoice :: MonadIO m
           => ChoiceName      -- ^ The name of the choice made.
           -> Party           -- ^ The party making the choice.
           -> ChosenNum       -- ^ The number chosen.
           -> Maybe FilePath  -- ^ The output JSON file representing the input.
           -> m ()            -- ^ Action to write the input to the file.
makeChoice name party chosen outputFile =
  maybeWriteJson outputFile
    . NormalInput
    $ IChoice (ChoiceId name party) chosen


-- | Serialise a notification input to a file.
makeNotification :: MonadIO m
                 => Maybe FilePath  -- ^ The output JSON file representing the input.
                 -> m ()            -- ^ Action to write the input to the file.
makeNotification outputFile =
  maybeWriteJson outputFile
    $ NormalInput INotify


-- | Create an initial Marlowe transaction.
initializeTransaction :: MonadError CliError m
              => MonadIO m
              => (Integer, POSIXTime)   -- ^ The slot length, in milliseconds, and the effective POSIX time of slot zero, in milliseconds.
              -> MarloweParams          -- ^ The Marlowe contract parameters.
              -> CostModelParams        -- ^ The cost model parameters.
              -> NetworkId              -- ^ The network ID.
              -> StakeAddressReference  -- ^ The stake address.
              -> FilePath               -- ^ The JSON file containing the contract.
              -> FilePath               -- ^ The JSON file containing the contract's state.
              -> Maybe FilePath         -- ^ The output JSON file for the validator information.
              -> Bool                   -- ^ Whether to print statistics about the validator.
              -> m ()                   -- ^ Action to export the validator information to a file.
initializeTransaction slotConfigFix marloweParams costModelParams network stake contractFile stateFile outputFile printStats =
  do
    contract <- decodeFileStrict contractFile
    state    <- decodeFileStrict stateFile
    initializeTransactionImpl
      slotConfigFix
      marloweParams costModelParams network stake
      contract state
      outputFile
      printStats


-- | Create an initial Marlowe transaction.
initializeTransactionImpl :: MonadError CliError m
                          => MonadIO m
                          => (Integer, POSIXTime)   -- ^ The slot length, in milliseconds, and the effective POSIX time of slot zero, in milliseconds.
                          -> MarloweParams          -- ^ The Marlowe contract parameters.
                          -> CostModelParams        -- ^ The cost model parameters.
                          -> NetworkId              -- ^ The network ID.
                          -> StakeAddressReference  -- ^ The stake address.
                          -> Contract               -- ^ The initial Marlowe contract.
                          -> State                  -- ^ The initial Marlowe state.
                          -> Maybe FilePath         -- ^ The output JSON file for the validator information.
                          -> Bool                   -- ^ Whether to print statistics about the validator.
                          -> m ()                   -- ^ Action to export the validator information to a file.
initializeTransactionImpl mtSlotConfigFix marloweParams costModelParams network stake mtContract mtState outputFile printStats =
  do
     let
       mtRoles = rolesCurrency marloweParams
     mtValidator <- liftCli $ buildValidator marloweParams costModelParams network stake
     mtRoleValidator <- liftCli $ buildRoleValidator mtRoles costModelParams network stake
     let
       ValidatorInfo{..} = mtValidator :: ValidatorInfo AlonzoEra  -- FIXME: Generalize eras.
       mtRange    = Nothing
       mtInputs   = []
       mtPayments = []
     maybeWriteJson outputFile MarloweTransaction{..}
     liftIO
       $ when printStats
         $ do
           hPutStrLn stderr ""
           hPutStrLn stderr $ "Validator size: " <> show viSize
           hPutStrLn stderr $ "Base-validator cost: " <> show viCost


-- | Prepare the next step in a Marlowe contract.
prepareTransaction :: MonadError CliError m
               => MonadIO m
               => FilePath        -- ^ The JSON file with the Marlowe initial state and initial contract.
               -> [Input]         -- ^ The JSON files containing the contract's inputs.
               -> SlotNo          -- ^ The first valid slot for the transaction.
               -> SlotNo          -- ^ The last valid slot for the transaction.
               -> Maybe FilePath  -- ^ The output JSON file with the results of the computation.
               -> Bool            -- ^ Whether to print statistics about the result.
               -> m ()            -- ^ Action to compute the next step in the contract.
prepareTransaction marloweFile txInputs (SlotNo minimumSlot) (SlotNo maximumSlot) outputFile printStats =
  do
    marloweIn <- decodeFileStrict marloweFile
    let
      txInterval = (fromIntegral minimumSlot, fromIntegral maximumSlot)
    (warnings, marloweOut@MarloweTransaction{..}) <-
      makeMarlowe
        (marloweIn :: MarloweTransaction AlonzoEra)  -- FIXME: Generalize eras.
        TransactionInput{..}
    maybeWriteJson outputFile marloweOut
    liftIO
      $ do
        when printStats
          $ do
            hPutStrLn stderr ""
            unless (null warnings)
              $ do
                hPutStrLn stderr "Warnings:"
                forM_ warnings
                  $ hPutStrLn stderr . ("  " <>) . show
            hPutStrLn stderr $ "Datum size: " <> show (diSize $ buildDatum mtSlotConfigFix mtContract mtState)
        sequence_
          [
            do
              putStrLn $ "Payment " <> show (i :: Int)
              putStrLn $ "  Acccount: " <> show accountId
              putStrLn $ "  Payee: " <> show payee
              putStrLn $ "  Ada: " <> show (getAda $ fromValue money)
              sequence_
                [
                  hPutStrLn stderr $ "  " <> show (pretty symbol) <> "." <> show (pretty token) <> ": " <> show amount
                |
                  (symbol, tokenAmounts) <- AM.toList $ getValue money
                , symbol /= adaSymbol
                , (token, amount) <- AM.toList tokenAmounts
                ]
          |
            (i, Payment accountId payee money) <- zip [1..] mtPayments
          ]


-- | Prepare the next step in a Marlowe contract.
makeMarlowe :: MonadError CliError m
            => MarloweTransaction era                            -- ^ The Marlowe initial state and initial contract.
            -> TransactionInput                                  -- ^ The transaction input.
            -> m ([TransactionWarning], MarloweTransaction era)  -- ^ Action to compute the next step in the contract.
makeMarlowe marloweIn@MarloweTransaction{..} transactionInput@TransactionInput{..} =
  do
    case computeTransaction transactionInput mtState mtContract of
      Error message          -> throwError . CliError . show $ message
      TransactionOutput{..} -> pure
                                 ( txOutWarnings
                                 , marloweIn
                                   {
                                     mtState    = txOutState
                                   , mtContract = txOutContract
                                   , mtRange    = Just $ bimap convertSlot convertSlot txInterval
                                   , mtInputs   = txInputs
                                   , mtPayments = txOutPayments
                                   }
                                 )
                                   where
                                     convertSlot = SlotNo . fromIntegral . getSlot


-- | Run a Marlowe transaction.
runTransaction :: MonadError CliError m
               => MonadIO m
               => LocalNodeConnectInfo CardanoMode        -- ^ The connection info for the local node.
               -> Maybe (FilePath, TxIn, TxIn)            -- ^ The JSON file with the Marlowe initial state and initial contract, along with the script eUTxO being spent and the collateral, unless the transaction opens the contract.
               -> FilePath                                -- ^ The JSON file with the Marlowe inputs, final state, and final contract.
               -> [TxIn]                                  -- ^ The transaction inputs.
               -> [(AddressAny, Maybe Datum, Api.Value)]  -- ^ The transaction outputs.
               -> AddressAny                              -- ^ The change address.
               -> [FilePath]                              -- ^ The files for required signing keys.
               -> FilePath                                -- ^ The output file for the transaction body.
               -> Maybe Int                               -- ^ Number of seconds to wait for the transaction to be confirmed, if it is to be confirmed.
               -> Bool                                    -- ^ Whether to print statistics about the transaction.
               -> Bool                                    -- ^ Assertion that the transaction is invalid.
               -> m TxId                                  -- ^ Action to build the transaction body.
runTransaction connection marloweInBundle marloweOutFile inputs outputs changeAddress signingKeyFiles bodyFile timeout printStats invalid =
  do
    protocol <- queryAlonzo connection QueryProtocolParameters
    marloweOut <- decodeFileStrict marloweOutFile
    (spend, collateral) <-
      case marloweInBundle of
        Nothing                                 -> pure ([], Nothing)
        Just (marloweInFile, spend, collateral) -> do
                                                    marloweIn  <- decodeFileStrict marloweInFile
                                                    let
                                                      validatorInfo = mtValidator (marloweIn :: MarloweTransaction AlonzoEra) -- FIXME: Generalize eras.
                                                      PlutusScript _ validator = viScript validatorInfo
                                                      redeemer = riRedeemer $ buildRedeemer (mtInputs marloweOut)
                                                      inputDatum  = diDatum $ buildDatum (mtSlotConfigFix marloweIn) (mtContract marloweIn) (mtState marloweIn)
                                                      spend' = buildPayFromScript validator inputDatum redeemer spend
                                                    pure ([spend'], Just collateral)
    let
      toAddressAny' :: AddressInEra AlonzoEra -> AddressAny
      toAddressAny' (AddressInEra _ address) = toAddressAny address
      network = localNodeNetworkId connection
      scriptAddress = viAddress $ mtValidator marloweOut
      outputDatum = diDatum $ buildDatum (mtSlotConfigFix marloweOut) (mtContract marloweOut) (mtState marloweOut)
    outputValue <-
      mconcat
        <$> sequence
        [
          liftCli . toCardanoValue $ assetClassValue (AssetClass (currency, name)) amount
        |
          ((_, Token currency name), amount) <- AM.toList . accounts $ mtState marloweOut
        ]
    let
      continue =
        do
          guard (outputValue /= mempty)
          pure
            $ buildPayToScript scriptAddress outputValue outputDatum
      roleAddress = viAddress $ mtRoleValidator marloweOut :: AddressInEra AlonzoEra
    payments <-
      catMaybes
      <$> sequence
        [
          case payee of
            Party (PK pkh)    -> do
                                   address <-
                                     liftCli
                                       . toCardanoAddress network
                                       $ Address (PubKeyCredential pkh) Nothing
                                   money' <-
                                     liftCli
                                       $ toCardanoValue money
                                   money'' <- adjustMinimumUTxO protocol address Nothing money'
                                   pure $ Just (toAddressAny' address, Nothing, money'')
            Party (Role role) -> do
                                   money' <-
                                     liftCli
                                       $ toCardanoValue money
                                   let
                                     datum = Just . diDatum $ buildRoleDatum role
                                   money'' <- adjustMinimumUTxO protocol roleAddress datum money'
                                   pure $ Just (toAddressAny' roleAddress, datum, money'')

            Account _         -> pure Nothing
        |
           (payee, money) <- bimap head mconcat
                               . unzip
                               <$> groupBy ((==) `on` fst)
                               [
                                 (payee, money)
                               | Payment _ payee money <- mtPayments marloweOut
                               ]
        ]
    let
      outputs' = payments <> outputs
    signingKeys <- mapM readSigningKey signingKeyFiles
    body <-
      buildBody connection
        spend continue
        inputs outputs'
        collateral changeAddress
        (mtRange marloweOut)
        (hashSigningKey <$> signingKeys)
        printStats
        invalid
    liftCliIO
      $ writeFileTextEnvelope bodyFile Nothing body
    forM_ timeout
      $ if invalid
          then const $ throwError "Refusing to submit an invalid transaction: collateral would be lost."
          else submitBody connection body signingKeys
    pure
      $ getTxId body


-- | Adjust the lovelace in an output to confirm to the minimum ADA requirement.
adjustMinimumUTxO :: MonadError CliError m
                  => ProtocolParameters       -- ^ The protocol parameters.
                  -> AddressInEra AlonzoEra   -- ^ The output address.
                  -> Maybe Datum              -- ^ The datum, if any.
                  -> Api.Value                -- ^ The output value.
                  -> m Api.Value              -- ^ Action to compute the adjusted value.
adjustMinimumUTxO protocol address datum value =
  do
    let
      txOut =
        TxOut
          address
          (TxOutValue MultiAssetInAlonzoEra value)
          (maybe TxOutDatumNone (TxOutDatum ScriptDataInAlonzoEra . fromPlutusData . toData) datum)
    minValue <- liftCli $ calculateMinimumUTxO ShelleyBasedEraAlonzo txOut protocol
    let
      minLovelace = selectLovelace minValue
      deficit = minLovelace <> negate (minimum[selectLovelace value, minLovelace])
    pure
      $ value <> lovelaceToValue deficit


-- | Withdraw funds for a specific role from the role address.
withdrawFunds :: MonadError CliError m
              => MonadIO m
              => LocalNodeConnectInfo CardanoMode        -- ^ The connection info for the local node.
              -> FilePath                                -- ^ The JSON file with the Marlowe state and contract.
              -> TokenName                               -- ^ The role name for the redemption.
              -> TxIn                                    -- ^ The collateral.
              -> [TxIn]                                  -- ^ The transaction inputs.
              -> [(AddressAny, Maybe Datum, Api.Value)]  -- ^ The transaction outputs.
              -> AddressAny                              -- ^ The change address.
              -> [FilePath]                              -- ^ The files for required signing keys.
              -> FilePath                                -- ^ The output file for the transaction body.
              -> Maybe Int                               -- ^ Number of seconds to wait for the transaction to be confirmed, if it is to be confirmed.
              -> Bool                                    -- ^ Whether to print statistics about the transaction.
              -> Bool                                    -- ^ Assertion that the transaction is invalid.
              -> m TxId                                  -- ^ Action to build the transaction body.
withdrawFunds connection marloweOutFile roleName collateral inputs outputs changeAddress signingKeyFiles bodyFile timeout printStats invalid =
  do
    marloweOut <- decodeFileStrict marloweOutFile
    signingKeys <- mapM readSigningKey signingKeyFiles
    roleHash <- liftCli . toCardanoScriptDataHash . diHash $ buildRoleDatum roleName
    let
      toAddressAny' :: AddressInEra AlonzoEra -> AddressAny
      toAddressAny' (AddressInEra _ address) = toAddressAny address
      validatorInfo = mtRoleValidator marloweOut
      PlutusScript _ roleScript = viScript validatorInfo
      roleAddress = viAddress validatorInfo :: AddressInEra AlonzoEra
      roleDatum = diDatum $ buildRoleDatum roleName
      roleRedeemer = riRedeemer buildRoleRedeemer
      checkRole (TxOut _ _ datum) =
        case datum of
          TxOutDatumNone             -> False
          TxOutDatumHash _ datumHash -> datumHash == roleHash
    utxos <-
      fmap (filter (checkRole . snd) . M.toList . unUTxO)
        . queryAlonzo connection
        . QueryUTxO
        . QueryUTxOByAddress
        . S.singleton
        . toAddressAny'
        $ roleAddress
    let
      spend = buildPayFromScript roleScript roleDatum roleRedeemer . fst <$> utxos
      withdrawal = (changeAddress, Nothing, mconcat [txOutValueToValue value | (_, TxOut _ value _) <- utxos])
      outputs' = withdrawal : outputs
    body <-
      buildBody connection
        spend Nothing
        inputs outputs'
        (Just collateral) changeAddress
        (mtRange marloweOut)
        (hashSigningKey <$> signingKeys)
        printStats
        invalid
    liftCliIO
      $ writeFileTextEnvelope bodyFile Nothing body
    forM_ timeout
      $ if invalid
          then const $ throwError "Refusing to submit an invalid transaction: collateral would be lost."
          else submitBody connection body signingKeys
    pure
      $ getTxId body
