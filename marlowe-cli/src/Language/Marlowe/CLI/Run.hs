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


{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}


module Language.Marlowe.CLI.Run (
-- * Computation
  initializeTransaction
, initializeTransactionImpl
, prepareTransaction
, prepareTransactionImpl
, makeMarlowe
, runTransaction
, autoRunTransaction
, runTransactionImpl
-- * Roles
, withdrawFunds
, autoWithdrawFunds
-- * Input
, makeDeposit
, makeChoice
, makeNotification
) where


import Cardano.Api (AddressInEra (..), CardanoMode, LocalNodeConnectInfo (localNodeNetworkId), NetworkId,
                    PaymentCredential (PaymentCredentialByKey),
                    QueryInShelleyBasedEra (QueryProtocolParameters, QueryUTxO), QueryUTxOFilter (QueryUTxOByAddress),
                    Script (..), ScriptDataSupportedInEra (..), SlotNo (..), StakeAddressReference (..), TxBody, TxId,
                    TxIn, TxMetadataInEra, TxMintValue (TxMintNone), TxOut (..), TxOutDatum (..), TxOutValue (..),
                    UTxO (..), calculateMinimumUTxO, getTxId, lovelaceToValue, makeShelleyAddressInEra, selectLovelace,
                    shelleyBasedEra, txOutValueToValue, writeFileTextEnvelope)
import qualified Cardano.Api as Api (Value)
import Cardano.Api.Shelley (ProtocolParameters, ReferenceScript (ReferenceScriptNone), fromPlutusData)
import Control.Monad (forM_, guard, unless, when)
import Control.Monad.Except (MonadError, MonadIO, catchError, liftIO, throwError)
import Control.Monad.Reader (MonadReader)
import Data.Bifunctor (bimap)
import Data.Function (on)
import Data.List (groupBy, sortBy)
import qualified Data.Map.Strict as M (toList)
import Data.Maybe (catMaybes, fromMaybe)
import qualified Data.Set as S (singleton)
import Language.Marlowe.CLI.Export (buildDatum, buildRedeemer, buildRoleDatum, buildRoleRedeemer, buildRoleValidator,
                                    buildValidator)
import Language.Marlowe.CLI.IO (decodeFileStrict, liftCli, liftCliIO, maybeWriteJson, readMaybeMetadata, readSigningKey)
import Language.Marlowe.CLI.Merkle (merkleizeInputs, merkleizeMarlowe)
import Language.Marlowe.CLI.Orphans ()
import Language.Marlowe.CLI.Transaction (buildBody, buildPayFromScript, buildPayToScript, ensureMinUtxo, hashSigningKey,
                                         queryInEra, selectCoins, submitBody)
import Language.Marlowe.CLI.Types (CliEnv, CliError (..), DatumInfo (..), MarloweTransaction (..), RedeemerInfo (..),
                                   SomeMarloweTransaction (..), SomePaymentSigningKey, ValidatorInfo (..), askEra,
                                   doWithCardanoEra, toAddressAny', toMultiAssetSupportedInEra, withShelleyBasedEra)
import Language.Marlowe.Core.V1.Semantics (MarloweParams (rolesCurrency), Payment (..), TransactionInput (..),
                                           TransactionOutput (..), TransactionWarning, computeTransaction)
import Language.Marlowe.Core.V1.Semantics.Types (AccountId, ChoiceId (..), ChoiceName, ChosenNum, Contract, Input (..),
                                                 InputContent (..), Party (..), Payee (..), State (accounts),
                                                 Token (..), getInputContent)
import Ledger.Address (PaymentPubKeyHash (PaymentPubKeyHash))
import Ledger.Tx.CardanoAPI (toCardanoPaymentKeyHash, toCardanoScriptDataHash, toCardanoValue)
import Plutus.V1.Ledger.Ada (adaSymbol, adaToken, fromValue, getAda)
import Plutus.V1.Ledger.Api (CostModelParams, Datum (..), POSIXTime, TokenName, toBuiltinData, toData)
import Plutus.V1.Ledger.SlotConfig (SlotConfig, posixTimeToEnclosingSlot)
import Plutus.V1.Ledger.Value (AssetClass (..), Value (..), assetClassValue, singleton)
import qualified PlutusTx.AssocMap as AM (toList)
import Prettyprinter.Extras (Pretty (..))
import System.IO (hPutStrLn, stderr)


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
              => MonadReader (CliEnv era) m
              => MarloweParams          -- ^ The Marlowe contract parameters.
              -> SlotConfig             -- ^ The POSIXTime-to-slot configuration.
              -> CostModelParams        -- ^ The cost model parameters.
              -> NetworkId              -- ^ The network ID.
              -> StakeAddressReference  -- ^ The stake address.
              -> FilePath               -- ^ The JSON file containing the contract.
              -> FilePath               -- ^ The JSON file containing the contract's state.
              -> Maybe FilePath         -- ^ The output JSON file for the validator information.
              -> Bool                   -- ^ Whether to deeply merkleize the contract.
              -> Bool                   -- ^ Whether to print statistics about the validator.
              -> m ()                   -- ^ Action to export the validator information to a file.
initializeTransaction marloweParams slotConfig costModelParams network stake contractFile stateFile outputFile merkleize printStats =
  do
    contract <- decodeFileStrict contractFile
    state    <- decodeFileStrict stateFile
    marloweTransaction <- initializeTransactionImpl
      marloweParams
      slotConfig
      costModelParams
      network
      stake
      contract
      state
      merkleize
      printStats
    era <- askEra
    maybeWriteJson outputFile $ SomeMarloweTransaction era marloweTransaction


-- | Create an initial Marlowe transaction.
initializeTransactionImpl :: forall m era
                           . MonadError CliError m
                          => MonadIO m
                          => MonadReader (CliEnv era) m
                          => MarloweParams                      -- ^ The Marlowe contract parameters.
                          -> SlotConfig                         -- ^ The POSIXTime-to-slot configuration.
                          -> CostModelParams                    -- ^ The cost model parameters.
                          -> NetworkId                          -- ^ The network ID.
                          -> StakeAddressReference              -- ^ The stake address.
                          -> Contract                           -- ^ The initial Marlowe contract.
                          -> State                              -- ^ The initial Marlowe state.
                          -> Bool                               -- ^ Whether to deeply merkleize the contract.
                          -> Bool                               -- ^ Whether to print statistics about the validator.
                          -> m (MarloweTransaction era)         -- ^ Action to return a MarloweTransaction
initializeTransactionImpl marloweParams mtSlotConfig costModelParams network stake mtContract mtState merkleize printStats =
  do
    era <- askEra
    let
      mtRoles = rolesCurrency marloweParams
    mtValidator <- liftCli $ buildValidator marloweParams era costModelParams network stake
    mtRoleValidator <- liftCli $ buildRoleValidator mtRoles era costModelParams network stake
    let
      ValidatorInfo{..} = mtValidator
      mtContinuations = mempty
      mtRange         = Nothing
      mtInputs        = []
      mtPayments      = []
    liftIO
      $ when printStats
        $ do
          hPutStrLn stderr ""
          hPutStrLn stderr $ "Validator size: " <> show viSize
          hPutStrLn stderr $ "Base-validator cost: " <> show viCost
    let marloweTransaction = MarloweTransaction{..}
    pure $ if merkleize then merkleizeMarlowe marloweTransaction else marloweTransaction


-- | Prepare the next step in a Marlowe contract.
prepareTransaction :: MonadError CliError m
               => MonadIO m
               => FilePath        -- ^ The JSON file with the Marlowe initial state and initial contract.
               -> [Input]         -- ^ The contract's inputs.
               -> POSIXTime       -- ^ The first valid time for the transaction.
               -> POSIXTime       -- ^ The last valid time for the transaction.
               -> Maybe FilePath  -- ^ The output JSON file with the results of the computation.
               -> Bool            -- ^ Whether to print statistics about the result.
               -> m ()            -- ^ Action to compute the next step in the contract.
prepareTransaction marloweFile txInputs minimumTime maximumTime outputFile printStats =
  do
    SomeMarloweTransaction era marloweIn <- decodeFileStrict marloweFile
    marloweOut <- prepareTransactionImpl marloweIn txInputs minimumTime maximumTime printStats
    maybeWriteJson outputFile $ SomeMarloweTransaction era marloweOut

-- | Implementation of Prepare function
prepareTransactionImpl :: MonadError CliError m
               => MonadIO m
               => MarloweTransaction era     -- ^ Marlowe transaction to be prepared.
               -> [Input]                    -- ^ The contract's inputs.
               -> POSIXTime                  -- ^ The first valid time for the transaction.
               -> POSIXTime                  -- ^ The last valid time for the transaction.
               -> Bool                       -- ^ Whether to print statistics about the result.
               -> m (MarloweTransaction era) -- ^ Action to compute the next step in the contract.
prepareTransactionImpl marloweIn txInputs minimumTime maximumTime printStats =
  do
    let
      txInterval = (minimumTime, maximumTime)
    (warnings, marloweOut@MarloweTransaction{..}) <- makeMarlowe marloweIn (TransactionInput txInterval txInputs)
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
            hPutStrLn stderr $ "Datum size: " <> show (diSize $ buildDatum mtContract mtState)
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
    pure marloweOut


-- | Prepare the next step in a Marlowe contract.
makeMarlowe :: MonadError CliError m
            => MarloweTransaction era                            -- ^ The Marlowe initial state and initial contract.
            -> TransactionInput                                  -- ^ The transaction input.
            -> m ([TransactionWarning], MarloweTransaction era)  -- ^ Action to compute the next step in the contract.
makeMarlowe marloweIn@MarloweTransaction{..} transactionInput =
  do
    transactionInput'@TransactionInput{..} <-
      merkleizeInputs marloweIn transactionInput
        `catchError` (const $ pure transactionInput)  -- TODO: Consider not catching errors here.
    case computeTransaction transactionInput' mtState mtContract of
      Error message          -> throwError . CliError . show $ message
      TransactionOutput{..} -> pure
                                 (
                                   txOutWarnings
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
                                     convertSlot = SlotNo . fromIntegral . posixTimeToEnclosingSlot mtSlotConfig

-- | Run a Marlowe transaction.
runTransaction :: forall era m
                . MonadError CliError m
               => MonadIO m
               => MonadReader (CliEnv era) m
               => LocalNodeConnectInfo CardanoMode        -- ^ The connection info for the local node.
               -> Maybe (FilePath, TxIn, TxIn)            -- ^ The JSON file with the Marlowe initial state and initial contract, along with the script eUTxO being spent and the collateral, unless the transaction opens the contract.
               -> FilePath                                -- ^ The JSON file with the Marlowe inputs, final state, and final contract.
               -> [TxIn]                                  -- ^ The transaction inputs.
               -> [(AddressInEra era, Maybe Datum, Api.Value)]  -- ^ The transaction outputs.
               -> AddressInEra era                              -- ^ The change address.
               -> [FilePath]                              -- ^ The files for required signing keys.
               -> Maybe FilePath                          -- ^ The file containing JSON metadata, if any.
               -> FilePath                                -- ^ The output file for the transaction body.
               -> Maybe Int                               -- ^ Number of seconds to wait for the transaction to be confirmed, if it is to be confirmed.
               -> Bool                                    -- ^ Whether to print statistics about the transaction.
               -> Bool                                    -- ^ Assertion that the transaction is invalid.
               -> m TxId                                  -- ^ Action to build the transaction body.
runTransaction connection marloweInBundle marloweOutFile inputs outputs changeAddress signingKeyFiles metadataFile bodyFile timeout printStats invalid =
  do
    metadata <- readMaybeMetadata metadataFile
    SomeMarloweTransaction era' marloweOut' <- decodeFileStrict marloweOutFile
    era <- askEra @era
    signingKeys <- mapM readSigningKey signingKeyFiles
    let marloweInBundle' =
          case marloweInBundle of
            Nothing                                 -> pure Nothing
            Just (marloweInFile, spend, collateral) -> do
                                                        SomeMarloweTransaction _ marloweIn  <- decodeFileStrict marloweInFile
                                                        pure $ Just (marloweIn, spend, collateral)

    body <- case (era, era') of
      (ScriptDataInAlonzoEra, ScriptDataInBabbageEra)  -> throwError "Running in Alonzo era, read file in Babbage era"
      (ScriptDataInBabbageEra, ScriptDataInAlonzoEra)  -> throwError "Running in Babbage era, read file in Alonzo era"
      (ScriptDataInAlonzoEra, ScriptDataInAlonzoEra)   -> runTransactionImpl connection marloweInBundle' marloweOut' inputs outputs changeAddress signingKeys metadata timeout printStats invalid
      (ScriptDataInBabbageEra, ScriptDataInBabbageEra) -> runTransactionImpl connection marloweInBundle' marloweOut' inputs outputs changeAddress signingKeys metadata timeout printStats invalid

    doWithCardanoEra $ liftCliIO $ writeFileTextEnvelope bodyFile Nothing body
    pure $ getTxId body

-- | Run a Marlowe transaction.
runTransactionImpl :: forall era m
                . MonadError CliError m
               => MonadIO m
               => MonadReader (CliEnv era) m
               => LocalNodeConnectInfo CardanoMode        -- ^ The connection info for the local node.
               -> Maybe (MarloweTransaction era, TxIn, TxIn)            -- ^ The JSON file with the Marlowe initial state and initial contract, along with the script eUTxO being spent and the collateral, unless the transaction opens the contract.
               -> MarloweTransaction era                                -- ^ The JSON file with the Marlowe inputs, final state, and final contract.
               -> [TxIn]                                  -- ^ The transaction inputs.
               -> [(AddressInEra era, Maybe Datum, Api.Value)]  -- ^ The transaction outputs.
               -> AddressInEra era                              -- ^ The change address.
               -> [SomePaymentSigningKey]                              -- ^ The files for required signing keys.
               -> TxMetadataInEra era                          -- ^ The file containing JSON metadata, if any.
               -> Maybe Int                               -- ^ Number of seconds to wait for the transaction to be confirmed, if it is to be confirmed.
               -> Bool                                    -- ^ Whether to print statistics about the transaction.
               -> Bool                                    -- ^ Assertion that the transaction is invalid.
               -> m (TxBody era)                          -- ^ Action to build the transaction body.
runTransactionImpl connection marloweInBundle marloweOut' inputs outputs changeAddress signingKeys metadata timeout printStats invalid =
  do
    protocol <- queryInEra connection QueryProtocolParameters
    era <- askEra @era
    let
      go :: MarloweTransaction era -> m (TxBody era)
      go marloweOut = do
        (spend, collateral, datumOutputs) <-
          case marloweInBundle of
            Nothing                                 -> pure ([], Nothing, [])
            Just (marloweIn, spend, collateral) -> do
                                                        let
                                                          validatorInfo = mtValidator marloweIn
                                                          PlutusScript _ validator = viScript validatorInfo
                                                          redeemer = riRedeemer $ buildRedeemer (mtInputs marloweOut)
                                                          inputDatum  = diDatum $ buildDatum (mtContract marloweIn) (mtState marloweIn)
                                                          spend' = buildPayFromScript validator inputDatum redeemer spend
                                                          -- SCP-3610: Remove when Babbage era features become available and the validator is revised.
                                                          merkles =
                                                            catMaybes
                                                              [
                                                                case input of
                                                                  NormalInput     _                -> Nothing
                                                                  MerkleizedInput _ _ continuation -> Just
                                                                                                      (
                                                                                                        -- Send the ancillary datum to the change address.
                                                                                                        changeAddress
                                                                                                        -- Astonishing that this eUTxO can be spent without script or redeemer!
                                                                                                      , Just . Datum $ toBuiltinData continuation
                                                                                                        -- FIXME: Replace with protocol-dependent min-Ada.
                                                                                                      , lovelaceToValue 1_500_000
                                                                                                      )
                                                              |
                                                                input <- mtInputs marloweOut
                                                              ]
                                                        pure ([spend'], Just collateral, merkles)
        let
          network = localNodeNetworkId connection

          scriptAddress = viAddress $ mtValidator marloweOut

          outputDatum = diDatum $ buildDatum (mtContract marloweOut) (mtState marloweOut)
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

          roleAddress = viAddress $ mtRoleValidator marloweOut
        payments <-
          catMaybes
          <$> sequence
            [
              case payee of
                Party (PK pkh)    -> do
                                      address <-
                                        liftCli $
                                          withShelleyBasedEra era $ makeShelleyAddressInEra network
                                            <$> (PaymentCredentialByKey <$> toCardanoPaymentKeyHash (PaymentPubKeyHash pkh))
                                            <*> pure NoStakeAddress
                                      money' <-
                                        liftCli
                                          $ toCardanoValue money
                                      money'' <- adjustMinimumUTxO era protocol address Nothing money'
                                      pure $ Just (address, Nothing, money'')
                Party (Role role) -> do
                                      money' <-
                                        liftCli
                                          $ toCardanoValue money
                                      let
                                        datum = Just . diDatum $ buildRoleDatum role
                                      money'' <- adjustMinimumUTxO era protocol roleAddress datum money'
                                      pure $ Just (roleAddress, datum, money'')

                Account _         -> pure Nothing
            |
              (payee, money) <- bimap head mconcat . unzip
                                  <$> (groupBy ((==) `on` fst) . sortBy (compare `on` fst))
                                  [
                                    (payee, money)
                                  | Payment _ payee money <- mtPayments marloweOut
                                  ]
            ]
        let
          outputs' = payments <> outputs <> datumOutputs
        body <-
          buildBody connection
            spend continue
            [] inputs outputs'
            collateral changeAddress
            (mtRange marloweOut)
            (hashSigningKey <$> signingKeys)
            TxMintNone
            metadata
            printStats
            invalid
        forM_ timeout
          $ if invalid
              then const $ throwError "Refusing to submit an invalid transaction: collateral would be lost."
              else submitBody connection body signingKeys
        pure body
    go marloweOut'


-- | 2022-08 This function was written to compensate for a bug in Cardano's calculateMinimumUTxO. It's called by adjustMinimumUTxO below. We will eventually be able to remove it.
ensureAtLeastHalfAnAda :: Api.Value -> Api.Value
ensureAtLeastHalfAnAda origValue =
  if origLovelace < minLovelace
    then origValue <> lovelaceToValue (minLovelace - origLovelace)
    else origValue
  where
    origLovelace = selectLovelace origValue
    minLovelace = Lovelace 500_000

-- | Adjust the lovelace in an output to confirm to the minimum ADA requirement.
adjustMinimumUTxO :: forall m era
                   . MonadError CliError m
                  => ScriptDataSupportedInEra era
                  -> ProtocolParameters       -- ^ The protocol parameters.
                  -> AddressInEra era         -- ^ The output address.
                  -> Maybe Datum              -- ^ The datum, if any.
                  -> Api.Value                -- ^ The output value.
                  -> m Api.Value              -- ^ Action to compute the adjusted value.
adjustMinimumUTxO era protocol address datum origValue =
  do
    let
      value = ensureAtLeastHalfAnAda origValue
      txOut =
        TxOut
          address
          (TxOutValue (toMultiAssetSupportedInEra era) value)
          (maybe TxOutDatumNone (TxOutDatumInTx era . fromPlutusData . toData) datum)
          ReferenceScriptNone
    minValue <- liftCli $ calculateMinimumUTxO (withShelleyBasedEra era shelleyBasedEra) txOut protocol
    let
      minLovelace = selectLovelace minValue
      deficit = minLovelace <> negate (minimum[selectLovelace value, minLovelace])
    pure
      $ value <> lovelaceToValue deficit


-- | Withdraw funds for a specific role from the role address.
withdrawFunds :: (MonadError CliError m, MonadReader (CliEnv era) m)
              => MonadIO m
              => LocalNodeConnectInfo CardanoMode        -- ^ The connection info for the local node.
              -> FilePath                                -- ^ The JSON file with the Marlowe state and contract.
              -> TokenName                               -- ^ The role name for the redemption.
              -> TxIn                                    -- ^ The collateral.
              -> [TxIn]                                  -- ^ The transaction inputs.
              -> [(AddressInEra era, Maybe Datum, Api.Value)]  -- ^ The transaction outputs.
              -> AddressInEra era                              -- ^ The change address.
              -> [FilePath]                              -- ^ The files for required signing keys.
              -> Maybe FilePath                          -- ^ The file containing JSON metadata, if any.
              -> FilePath                                -- ^ The output file for the transaction body.
              -> Maybe Int                               -- ^ Number of seconds to wait for the transaction to be confirmed, if it is to be confirmed.
              -> Bool                                    -- ^ Whether to print statistics about the transaction.
              -> Bool                                    -- ^ Assertion that the transaction is invalid.
              -> m TxId                                  -- ^ Action to build the transaction body.
withdrawFunds connection marloweOutFile roleName collateral inputs outputs changeAddress signingKeyFiles metadataFile bodyFile timeout printStats invalid =
  do
    metadata <- readMaybeMetadata metadataFile
    SomeMarloweTransaction _ marloweOut <- decodeFileStrict marloweOutFile
    signingKeys <- mapM readSigningKey signingKeyFiles
    roleHash <- liftCli . toCardanoScriptDataHash . diHash $ buildRoleDatum roleName
    let
      validatorInfo = mtRoleValidator marloweOut
      PlutusScript _ roleScript = viScript validatorInfo
      roleAddress = viAddress validatorInfo
      roleDatum = diDatum $ buildRoleDatum roleName
      roleRedeemer = riRedeemer buildRoleRedeemer
      checkRole (TxOut _ _ datum _) =
        case datum of
          TxOutDatumInline _ _       -> False
          TxOutDatumNone             -> False
          TxOutDatumHash _ datumHash -> datumHash == roleHash
    utxos <-
      fmap (filter (checkRole . snd) . M.toList . unUTxO)
        . queryInEra connection
        . QueryUTxO
        . QueryUTxOByAddress
        . S.singleton
        . toAddressAny'
        $ roleAddress
    let
      spend = buildPayFromScript roleScript roleDatum roleRedeemer . fst <$> utxos
      withdrawal = (changeAddress, Nothing, mconcat [txOutValueToValue value | (_, TxOut _ value _ _) <- utxos])
      outputs' = withdrawal : outputs
    body <-
      buildBody connection
        spend Nothing
        [] inputs outputs'
        (Just collateral) changeAddress
        (mtRange marloweOut)
        (hashSigningKey <$> signingKeys)
        TxMintNone
        metadata
        printStats
        invalid
    doWithCardanoEra $ liftCliIO $ writeFileTextEnvelope bodyFile Nothing body
    forM_ timeout
      $ if invalid
          then const $ throwError "Refusing to submit an invalid transaction: collateral would be lost."
          else submitBody connection body signingKeys
    pure
      $ getTxId body


-- | Run a Marlowe transaction, without selecting inputs or outputs.
autoRunTransaction :: forall era m
                . MonadError CliError m
               => MonadIO m
               => MonadReader (CliEnv era) m
               => LocalNodeConnectInfo CardanoMode        -- ^ The connection info for the local node.
               -> Maybe (FilePath, TxIn)                  -- ^ The JSON file with the Marlowe initial state and initial contract, unless the transaction opens the contract.
               -> FilePath                                -- ^ The JSON file with the Marlowe inputs, final state, and final contract.
               -> AddressInEra era                        -- ^ The change address.
               -> [FilePath]                              -- ^ The files for required signing keys.
               -> Maybe FilePath                          -- ^ The file containing JSON metadata, if any.
               -> FilePath                                -- ^ The output file for the transaction body.
               -> Maybe Int                               -- ^ Number of seconds to wait for the transaction to be confirmed, if it is to be confirmed.
               -> Bool                                    -- ^ Whether to print statistics about the transaction.
               -> Bool                                    -- ^ Assertion that the transaction is invalid.
               -> m TxId                                  -- ^ Action to build the transaction body.
autoRunTransaction connection marloweInBundle marloweOutFile changeAddress signingKeyFiles metadataFile bodyFile timeout printStats invalid =
  do
    -- Fetch the protocol parameters.
    protocol <- queryInEra connection QueryProtocolParameters
    -- Read any metadata for the transaction.
    metadata <- readMaybeMetadata metadataFile
    -- Read the Marlowe transaction information for the output.
    SomeMarloweTransaction era' marloweOut' <- decodeFileStrict marloweOutFile
    -- Fetch the era.
    era <- askEra @era
    let
      go :: MarloweTransaction era -> m TxId
      go marloweOut = do
        -- Find the script UTxO to spend and the datum for outputs to the role payout address.
        (spend, datumOutputs) <-
          case marloweInBundle of
                                           -- This is a creation transaction.
            Nothing                     -> pure ([], [])
                                           -- This is a non-creation transaction.
            Just (marloweInFile, spend) -> do
                                            -- Find the results of the previous transaction.
                                            SomeMarloweTransaction _ marloweIn  <- decodeFileStrict marloweInFile
                                            let
                                              -- Fetch the validator.
                                              validatorInfo = mtValidator marloweIn
                                              PlutusScript _ validator = viScript validatorInfo
                                              -- Build the redeemer.
                                              redeemer = riRedeemer $ buildRedeemer (mtInputs marloweOut)
                                              -- Build the datum.
                                              inputDatum  = diDatum $ buildDatum (mtContract marloweIn) (mtState marloweIn)
                                              -- Build the spending witness.
                                              spend' = buildPayFromScript validator inputDatum redeemer spend
                                              -- Handle merkleization.
                                              merkles =
                                                catMaybes
                                                  [
                                                    case input of
                                                      NormalInput     _                -> Nothing
                                                      MerkleizedInput _ _ continuation -> Just
                                                                                          (
                                                                                            -- Send the ancillary datum to the change address.
                                                                                            changeAddress
                                                                                            -- Astonishing that this eUTxO can be spent without script or redeemer!
                                                                                          , Just . Datum $ toBuiltinData continuation
                                                                                            -- FIXME: Replace with protocol-dependent min-Ada.
                                                                                          , lovelaceToValue 1_500_000
                                                                                          )
                                                  |
                                                    input <- mtInputs marloweOut
                                                  ]
                                            -- Return the spending witness and the extra datum for demerkleization.
                                            pure ([spend'], merkles)
        let
          -- Get the type of network.
          network = localNodeNetworkId connection
          -- Compute the script address.
          scriptAddress = viAddress $ mtValidator marloweOut
          -- Build the datum output to the script.
          outputDatum = diDatum $ buildDatum (mtContract marloweOut) (mtState marloweOut)
        -- Determine how much value the script should receive.
        outputValue <-
          mconcat
            <$> sequence
            [
              liftCli . toCardanoValue $ assetClassValue (AssetClass (currency, name)) amount
            |
              ((_, Token currency name), amount) <- AM.toList . accounts $ mtState marloweOut
            ]
        let
          -- Build the continuing output, if any.
          continue =
            do
              guard (outputValue /= mempty)
              pure
                $ buildPayToScript scriptAddress outputValue outputDatum

          -- Compute the role-payout address.
          roleAddress = viAddress $ mtRoleValidator marloweOut
        -- Build the payments to the role-payout address.
        payments <-
          catMaybes
          <$> sequence
            [
              case payee of
                Party (PK pkh)    -> do
                                      address <-
                                        liftCli $
                                          withShelleyBasedEra era $ makeShelleyAddressInEra network
                                            <$> (PaymentCredentialByKey <$> toCardanoPaymentKeyHash (PaymentPubKeyHash pkh))
                                            <*> pure NoStakeAddress
                                      money' <- liftCli $ toCardanoValue money
                                      Just <$> ensureMinUtxo protocol (address, Nothing, money')
                Party (Role role) -> do
                                      money' <- liftCli $ toCardanoValue money
                                      let
                                        datum = Just . diDatum $ buildRoleDatum role
                                      Just <$> ensureMinUtxo protocol (roleAddress, datum, money')

                Account _         -> pure Nothing
            |
              (payee, money) <- bimap head mconcat . unzip
                                  <$> (groupBy ((==) `on` fst) . sortBy (compare `on` fst))
                                  [
                                    (payee, money)
                                  | Payment _ payee money <- mtPayments marloweOut
                                  ]
            ]
        -- Read the signing keys.
        signingKeys <- mapM readSigningKey signingKeyFiles
        let
          -- Extract the roles needed to authorize the inputs.
          roles [] = []
          roles (input : inputs) =
            case getInputContent input of
              IDeposit _ (Role role) _ _         -> role : roles inputs
              IChoice (ChoiceId _ (Role role)) _ -> role : roles inputs
              _                                  -> roles inputs
          -- Extract the value coming from a UTxO.
          incomingValue (txIn', TxOut _ value _ _)
            | txIn' `elem` fmap txIn spend = txOutValueToValue value
            | otherwise                    = mempty
        -- Compute the value coming from the script.
        incoming <-
          fmap (mconcat . fmap incomingValue . M.toList . unUTxO)
            . queryInEra connection
            . QueryUTxO
            . QueryUTxOByAddress
            . S.singleton
            . toAddressAny'
            $ scriptAddress
        -- Build the outputs for role tokens.
        roleOutputs <-
          sequence
            [
              do
                value <- liftCli . toCardanoValue $ singleton (mtRoles marloweOut) role 1
                ensureMinUtxo protocol (changeAddress, Nothing, value)
            |
              role <- roles $ mtInputs marloweOut
            ]
        -- Select the coins.
        (collateral, extraInputs, revisedOutputs)
          <- selectCoins
               connection
               incoming
               (payments <> roleOutputs <> datumOutputs)
               continue
               changeAddress
        -- Build the transaction body.
        body <-
          buildBody connection
            spend continue
            [] extraInputs revisedOutputs
            (Just collateral) changeAddress
            (mtRange marloweOut)
            (hashSigningKey <$> signingKeys)
            TxMintNone
            metadata
            printStats
            invalid
        -- Write the transaction file.
        doWithCardanoEra $ liftCliIO $ writeFileTextEnvelope bodyFile Nothing body
        -- Optionally submit the transaction, waiting for a timeout.
        forM_ timeout
          $ if invalid
              then const $ throwError "Refusing to submit an invalid transaction: collateral would be lost."
              else submitBody connection body signingKeys
        -- Return the transaction identifier.
        pure
          $ getTxId body
    -- Deal with era.
    case (era, era') of
      (ScriptDataInAlonzoEra, ScriptDataInBabbageEra)  -> throwError "Running in Alonzo era, read file in Babbage era"
      (ScriptDataInBabbageEra, ScriptDataInAlonzoEra)  -> throwError "Running in Babbage era, read file in Alonzo era"
      (ScriptDataInAlonzoEra, ScriptDataInAlonzoEra)   -> go marloweOut'
      (ScriptDataInBabbageEra, ScriptDataInBabbageEra) -> go marloweOut'



-- | Withdraw funds for a specific role from the role address, without selecting inputs or outputs.
autoWithdrawFunds :: (MonadError CliError m, MonadReader (CliEnv era) m)
                  => MonadIO m
                  => LocalNodeConnectInfo CardanoMode        -- ^ The connection info for the local node.
                  -> FilePath                                -- ^ The JSON file with the Marlowe state and contract.
                  -> TokenName                               -- ^ The role name for the redemption.
                  -> AddressInEra era                        -- ^ The change address.
                  -> [FilePath]                              -- ^ The files for required signing keys.
                  -> Maybe FilePath                          -- ^ The file containing JSON metadata, if any.
                  -> FilePath                                -- ^ The output file for the transaction body.
                  -> Maybe Int                               -- ^ Number of seconds to wait for the transaction to be confirmed, if it is to be confirmed.
                  -> Bool                                    -- ^ Whether to print statistics about the transaction.
                  -> Bool                                    -- ^ Assertion that the transaction is invalid.
                  -> m TxId                                  -- ^ Action to build the transaction body.
autoWithdrawFunds connection marloweOutFile roleName changeAddress signingKeyFiles metadataFile bodyFile timeout printStats invalid =
  do
    -- Fetch the protocol parameters.
    protocol <- queryInEra connection QueryProtocolParameters
    -- Read any metadata for the transaction.
    metadata <- readMaybeMetadata metadataFile
    -- Read the Marlowe transaction information that was used to populate the role-payout address.
    SomeMarloweTransaction _ marloweOut <- decodeFileStrict marloweOutFile
    -- Read the signing keys.
    signingKeys <- mapM readSigningKey signingKeyFiles
    -- Compute the hash of the role name.
    roleHash <- liftCli . toCardanoScriptDataHash . diHash $ buildRoleDatum roleName
    let
      -- Compute the validator information.
      validatorInfo = mtRoleValidator marloweOut
      -- Fetch the role-payout validator script.
      PlutusScript _ roleScript = viScript validatorInfo
      -- Fetch the role address.
      roleAddress = viAddress validatorInfo
      -- Build the datum corresponding to the role name.
      roleDatum = diDatum $ buildRoleDatum roleName
      -- Build the necessary redeemer.
      roleRedeemer = riRedeemer buildRoleRedeemer
      -- Test if a `TxOut` contains the role token datum.
      checkRole (TxOut _ _ datum _) =
        case datum of
          TxOutDatumInline _ _       -> False
          TxOutDatumNone             -> False
          TxOutDatumHash _ datumHash -> datumHash == roleHash
    -- Find the role token.
    utxos <-
      fmap (filter (checkRole . snd) . M.toList . unUTxO)
        . queryInEra connection
        . QueryUTxO
        . QueryUTxOByAddress
        . S.singleton
        . toAddressAny'
        $ roleAddress
    -- Set the value of one role token.
    role <- liftCli $ toCardanoValue $ singleton (mtRoles marloweOut) roleName 1
    let
      -- Build the spending from the script.
      spend = buildPayFromScript roleScript roleDatum roleRedeemer . fst <$> utxos
      -- Find how much is being spent from the script.
      -- The output value should include the spending from the script and the role token.
      withdrawn = mconcat [txOutValueToValue value | (_, TxOut _ value _ _) <- utxos]
    -- Ensure that the output meets the min-Ada requirement.
    output <- ensureMinUtxo protocol (changeAddress, Nothing, withdrawn <> role)
    -- Select the coins.
    (collateral, extraInputs, revisedOutputs) <-
      selectCoins
        connection
        withdrawn
        [output]
        Nothing
        changeAddress
    -- Build the transaction body.
    body <-
      buildBody connection
        spend Nothing
        [] extraInputs revisedOutputs
        (Just collateral) changeAddress
        (mtRange marloweOut)
        (hashSigningKey <$> signingKeys)
        TxMintNone
        metadata
        printStats
        invalid
    -- Write the transaction file.
    doWithCardanoEra $ liftCliIO $ writeFileTextEnvelope bodyFile Nothing body
    -- Optionally submit the transaction, waiting for a timeout.
    forM_ timeout
      $ if invalid
          then const $ throwError "Refusing to submit an invalid transaction: collateral would be lost."
          else submitBody connection body signingKeys
    -- Return the transaction identifier.
    pure
      $ getTxId body
