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
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}


module Language.Marlowe.CLI.Run (
-- * Computation
  adjustMinimumUTxO
,  initializeTransaction
, initializeTransactionImpl
, initializeTransactionUsingScriptRefsImpl
, prepareTransaction
, prepareTransactionImpl
, makeMarlowe
, autoRunTransaction
, autoRunTransactionImpl
, runTransaction
, runTransactionImpl
-- * Roles
, withdrawFunds
, autoWithdrawFunds
-- * Input
, makeDeposit
, makeChoice
, makeNotification
) where


import Cardano.Api (AddressInEra (..), CardanoMode, LocalNodeConnectInfo (..), NetworkId, PaymentCredential (..),
                    PlutusScriptVersion (PlutusScriptV1, PlutusScriptV2), QueryInShelleyBasedEra (..),
                    QueryUTxOFilter (..), ScriptDataSupportedInEra (..), SlotNo (..), StakeAddressReference (..),
                    TxBody, TxId, TxIn, TxMetadataInEra, TxMintValue (..), TxOut (..), TxOutDatum (..), UTxO (..),
                    getTxId, lovelaceToValue, makeShelleyAddressInEra, txOutValueToValue, writeFileTextEnvelope)
import qualified Cardano.Api as Api (Value)
import qualified Cardano.Api as C
import Cardano.Api.Shelley (ReferenceScript (ReferenceScriptNone))
import qualified Cardano.Api.Shelley as CS
import Control.Monad (forM_, guard, unless, when)
import Control.Monad.Except (MonadError, MonadIO, catchError, liftIO, throwError)
import Control.Monad.Reader (MonadReader)
import Data.Bifunctor (bimap)
import Data.Function (on)
import Data.List (groupBy, sortBy)
import qualified Data.Map.Strict as M (toList)
import Data.Maybe (catMaybes, fromMaybe)
import qualified Data.Set as S (singleton)
import Data.Traversable (for)
import Data.Tuple.Extra (uncurry3)
import Language.Marlowe.CLI.Cardano.Api (adjustMinimumUTxO)
import Language.Marlowe.CLI.Cardano.Api.Address (toShelleyStakeReference)
import Language.Marlowe.CLI.Cardano.Api.PlutusScript (IsPlutusScriptLanguage (plutusScriptVersion))
import Language.Marlowe.CLI.Export (buildMarloweDatum, buildRedeemer, buildRoleDatum, buildRoleRedeemer,
                                    marloweValidatorInfo, roleValidatorInfo)
import Language.Marlowe.CLI.IO (decodeFileStrict, liftCli, liftCliIO, maybeWriteJson, queryInEra, readMaybeMetadata,
                                readSigningKey)
import Language.Marlowe.CLI.Merkle (merkleizeInputs, merkleizeMarlowe)
import Language.Marlowe.CLI.Orphans ()
import Language.Marlowe.CLI.Transaction (buildBody, buildPayFromScript, buildPayToScript, ensureMinUtxo, hashSigningKey,
                                         makeTxOut', selectCoins, submitBody)
import Language.Marlowe.CLI.Types (CliEnv, CliError (..), DatumInfo (..), MarlowePlutusVersion,
                                   MarloweScriptsRefs (MarloweScriptsRefs, mrMarloweValidator, mrRolePayoutValidator),
                                   MarloweTransaction (..), PrintStats (PrintStats), RedeemerInfo (..),
                                   SigningKeyFile (..), SomeMarloweTransaction (..), SomePaymentSigningKey,
                                   TxBodyFile (TxBodyFile, unTxBodyFile), ValidatorInfo (..), askEra,
                                   defaultCoinSelectionStrategy, doWithCardanoEra, toAddressAny', toShelleyAddress,
                                   txIn, validatorInfoScriptOrReference, withShelleyBasedEra)
import qualified Language.Marlowe.Client as MC
import Language.Marlowe.Core.V1.Semantics (MarloweParams (rolesCurrency), Payment (..), TransactionInput (..),
                                           TransactionOutput (..), TransactionWarning, computeTransaction)
import Language.Marlowe.Core.V1.Semantics.Types (AccountId, ChoiceId (..), ChoiceName, ChosenNum, Contract, Input (..),
                                                 InputContent (..), Party (..), Payee (..), State (accounts),
                                                 Token (..), getInputContent)
import Ledger.Address (PaymentPubKeyHash (PaymentPubKeyHash))
import Ledger.Tx.CardanoAPI (toCardanoPaymentKeyHash, toCardanoScriptDataHash, toCardanoValue)
import Plutus.ApiCommon (ProtocolVersion)
import Plutus.V1.Ledger.Ada (fromValue)
import Plutus.V1.Ledger.SlotConfig (SlotConfig, posixTimeToEnclosingSlot)
import Plutus.V1.Ledger.Value (AssetClass (..), Value (..), assetClassValue, singleton)
import Plutus.V2.Ledger.Api (CostModelParams, Datum (..), POSIXTime, TokenName, adaSymbol, adaToken, toBuiltinData)
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
                      -> ProtocolVersion
                      -> CostModelParams        -- ^ The cost model parameters.
                      -> NetworkId              -- ^ The network ID.
                      -> StakeAddressReference  -- ^ The stake address.
                      -> FilePath               -- ^ The JSON file containing the contract.
                      -> FilePath               -- ^ The JSON file containing the contract's state.
                      -> Maybe FilePath         -- ^ The output JSON file for the validator information.
                      -> Bool                   -- ^ Whether to deeply merkleize the contract.
                      -> Bool                   -- ^ Whether to print statistics about the validator.
                      -> m ()                   -- ^ Action to export the validator information to a file.
initializeTransaction marloweParams slotConfig protocolVersion costModelParams network stake contractFile stateFile outputFile merkleize printStats =
  do
    contract <- decodeFileStrict contractFile
    state    <- decodeFileStrict stateFile
    marloweTransaction <- initializeTransactionImpl
      marloweParams
      slotConfig
      protocolVersion
      costModelParams
      network
      stake
      contract
      state
      merkleize
      printStats
    era <- askEra
    maybeWriteJson outputFile $
      SomeMarloweTransaction
      (plutusScriptVersion :: PlutusScriptVersion MarlowePlutusVersion)
      era
      marloweTransaction


-- | Create an initial Marlowe transaction.
initializeTransactionImpl :: forall m era
                           . MonadError CliError m
                          => MonadIO m
                          => MonadReader (CliEnv era) m
                          => MarloweParams                      -- ^ The Marlowe contract parameters.
                          -> SlotConfig                         -- ^ The POSIXTime-to-slot configuration.
                          -> ProtocolVersion
                          -> CostModelParams                    -- ^ The cost model parameters.
                          -> NetworkId                          -- ^ The network ID.
                          -> StakeAddressReference              -- ^ The stake address.
                          -> Contract                           -- ^ The initial Marlowe contract.
                          -> State                              -- ^ The initial Marlowe state.
                          -> Bool                               -- ^ Whether to deeply merkleize the contract.
                          -> Bool                               -- ^ Whether to print statistics about the validator.
                          -> m (MarloweTransaction MarlowePlutusVersion era)    -- ^ Action to return a MarloweTransaction
initializeTransactionImpl marloweParams mtSlotConfig protocolVersion costModelParams network stake mtContract mtState merkleize printStats =
  do
    era <- askEra
    let
      mtRolesCurrency = rolesCurrency marloweParams
    mtValidator <- liftCli $ marloweValidatorInfo era protocolVersion costModelParams network stake
    mtRoleValidator <- liftCli $ roleValidatorInfo era protocolVersion costModelParams network stake
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


-- | Create an initial Marlowe transaction using reference scripts.
initializeTransactionUsingScriptRefsImpl :: forall era lang m
                                          . MonadError CliError m
                                         => MonadIO m
                                         => C.IsShelleyBasedEra era
                                         => MarloweParams                      -- ^ The Marlowe contract parameters.
                                         -> SlotConfig                         -- ^ The POSIXTime-to-slot configuration.
                                         -> MarloweScriptsRefs lang era
                                         -> StakeAddressReference              -- ^ The stake address.
                                         -> Contract                           -- ^ The initial Marlowe contract.
                                         -> State                              -- ^ The initial Marlowe state.
                                         -> Bool                               -- ^ Whether to deeply merkleize the contract.
                                         -> Bool                               -- ^ Whether to print statistics about the validator.
                                         -> m (MarloweTransaction lang era)    -- ^ Action to return a MarloweTransaction
initializeTransactionUsingScriptRefsImpl marloweParams mtSlotConfig scriptRefs stake mtContract mtState merkleize printStats =
  do
    let
      mtRolesCurrency = rolesCurrency marloweParams
      setupStaking vi@ValidatorInfo { viAddress } = do
        case toShelleyAddress viAddress of
          Nothing -> throwError "Expecting shelley address in reference validator info"
          Just (CS.ShelleyAddress n p _) -> do
            let
              viAddress' = C.shelleyAddressInEra $ CS.ShelleyAddress
                n
                p
                (toShelleyStakeReference stake)
            pure $ vi { viAddress = viAddress' }

      MarloweScriptsRefs { mrMarloweValidator = (_, mv), mrRolePayoutValidator = (_, pv) } = scriptRefs

    mtValidator <- setupStaking mv
    mtRoleValidator <- setupStaking pv

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
    SomeMarloweTransaction lang era marloweIn <- decodeFileStrict marloweFile
    marloweOut <- prepareTransactionImpl marloweIn txInputs minimumTime maximumTime printStats
    maybeWriteJson outputFile $ SomeMarloweTransaction lang era marloweOut


-- | Implementation of Prepare function
prepareTransactionImpl :: MonadError CliError m
                       => MonadIO m
                       => MarloweTransaction lang era     -- ^ Marlowe transaction to be prepared.
                       -> [Input]                         -- ^ The contract's inputs.
                       -> POSIXTime                       -- ^ The first valid time for the transaction.
                       -> POSIXTime                       -- ^ The last valid time for the transaction.
                       -> Bool                            -- ^ Whether to print statistics about the result.
                       -> m (MarloweTransaction lang era) -- ^ Action to compute the next step in the contract.
prepareTransactionImpl marloweIn txInputs minimumTime maximumTime printStats =
  do
    let
      txInterval = (minimumTime, maximumTime)
      mp = MC.marloweParams $ mtRolesCurrency marloweIn
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
            hPutStrLn stderr $ "Datum size: " <> show (diSize $ buildMarloweDatum mp mtContract mtState)
        sequence_
          [
            do
              putStrLn $ "Payment " <> show (i :: Int)
              putStrLn $ "  Acccount: " <> show accountId
              putStrLn $ "  Payee: " <> show payee
              putStrLn $ "  Ada: " <> show (fromValue money)
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
            => MarloweTransaction lang era                            -- ^ The Marlowe initial state and initial contract.
            -> TransactionInput                                       -- ^ The transaction input.
            -> m ([TransactionWarning], MarloweTransaction lang era)  -- ^ Action to compute the next step in the contract.
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

readMarloweTransactionFile :: forall era lang m
                            . MonadError CliError m
                           => MonadIO m
                           => C.IsCardanoEra era
                           => PlutusScriptVersion lang
                           -> FilePath
                           -> m (MarloweTransaction lang era)
readMarloweTransactionFile lang marloweInFile = do
  SomeMarloweTransaction lang' era' marloweIn  <- decodeFileStrict marloweInFile
  case (C.cardanoEra :: C.CardanoEra era, era', lang, lang') of
    (C.AlonzoEra, ScriptDataInAlonzoEra, PlutusScriptV1, PlutusScriptV1)   -> pure marloweIn
    (C.BabbageEra, C.ScriptDataInBabbageEra, PlutusScriptV2, PlutusScriptV2) -> pure marloweIn
    (C.AlonzoEra, C.ScriptDataInBabbageEra, _, _)  -> throwError "Running in Alonzo era, read file in Babbage era"
    (C.BabbageEra, C.ScriptDataInAlonzoEra, _, _)  -> throwError "Running in Babbage era, read file in Alonzo era"
    (_, _, _, _)  -> throwError . CliError $
      "Expecting a different version of plutus in the Marlowe file. Expected: " <> show lang <> " but got: " <> show lang'


-- | Run a Marlowe transaction using FS.
runTransaction :: forall era m
                . MonadError CliError m
               => MonadIO m
               => MonadReader (CliEnv era) m
               => LocalNodeConnectInfo CardanoMode              -- ^ The connection info for the local node.
               -> Maybe (FilePath, TxIn, TxIn)                  -- ^ The JSON file with the Marlowe initial state and initial contract, along with the script eUTxO being spent and the collateral, unless the transaction opens the contract.
               -> FilePath                                      -- ^ The JSON file with the Marlowe inputs, final state, and final contract.
               -> [TxIn]                                        -- ^ The transaction inputs.
               -> [(AddressInEra era, Maybe Datum, Api.Value)]  -- ^ The transaction outputs.
               -> AddressInEra era                              -- ^ The change address.
               -> [SigningKeyFile]                              -- ^ The files for required signing keys.
               -> Maybe FilePath                                -- ^ The file containing JSON metadata, if any.
               -> TxBodyFile                                    -- ^ The output file for the transaction body.
               -> Maybe Int                                     -- ^ Number of seconds to wait for the transaction to be confirmed, if it is to be confirmed.
               -> Bool                                          -- ^ Whether to print statistics about the transaction.
               -> Bool                                          -- ^ Assertion that the transaction is invalid.
               -> m TxId                                        -- ^ Action to build the transaction body.
runTransaction connection marloweInBundle marloweOutFile inputs outputs changeAddress signingKeyFiles metadataFile (TxBodyFile bodyFile) timeout printStats invalid =
  do
    metadata <- readMaybeMetadata metadataFile
    SomeMarloweTransaction _ era' marloweOut' <- decodeFileStrict marloweOutFile
    era <- askEra @era
    signingKeys <- mapM readSigningKey signingKeyFiles
    let
      go :: forall lang. IsPlutusScriptLanguage lang => C.IsCardanoEra era => MarloweTransaction lang era -> m TxId
      go marloweOut'' = do
        marloweInBundle' <- case marloweInBundle of
          Nothing -> pure Nothing
          Just (marloweInFile, marloweTxIn, collateralTxIn) -> do
            marloweIn <- readMarloweTransactionFile (plutusScriptVersion :: PlutusScriptVersion lang) marloweInFile
            pure $ Just (marloweIn, marloweTxIn, collateralTxIn)

        (body :: TxBody era) <- runTransactionImpl connection marloweInBundle' marloweOut'' inputs outputs changeAddress signingKeys metadata timeout printStats invalid
        doWithCardanoEra $ liftCliIO $ writeFileTextEnvelope bodyFile Nothing body
        pure $ getTxId body

    case (era, era') of
      (ScriptDataInAlonzoEra, ScriptDataInAlonzoEra)   -> go marloweOut'
      (ScriptDataInBabbageEra, ScriptDataInBabbageEra) -> go marloweOut'
      (ScriptDataInAlonzoEra, ScriptDataInBabbageEra)  -> throwError "Running in Alonzo era, read file in Babbage era"
      (ScriptDataInBabbageEra, ScriptDataInAlonzoEra)  -> throwError "Running in Babbage era, read file in Alonzo era"


-- | Run a Marlowe transaction.
runTransactionImpl :: forall era lang m
                . MonadError CliError m
               => IsPlutusScriptLanguage lang
               => MonadIO m
               => MonadReader (CliEnv era) m
               => LocalNodeConnectInfo CardanoMode                      -- ^ The connection info for the local node.
               -> Maybe (MarloweTransaction lang era, TxIn, TxIn)     -- ^ The Marlowe initial state and initial contract, along with the script eUTxO being spent and the collateral, unless the transaction opens the contract.
               -> MarloweTransaction lang era                         -- ^ The JSON file with the Marlowe inputs, final state, and final contract.
               -> [TxIn]                                              -- ^ The transaction inputs.
               -> [(AddressInEra era, Maybe Datum, Api.Value)]        -- ^ The transaction outputs.
               -> AddressInEra era                                    -- ^ The change address.
               -> [SomePaymentSigningKey]                             -- ^ Required signing keys.
               -> TxMetadataInEra era                                 -- ^ Tx metadata.
               -> Maybe Int                                           -- ^ Number of seconds to wait for the transaction to be confirmed, if it is to be confirmed.
               -> Bool                                                -- ^ Whether to print statistics about the transaction.
               -> Bool                                                -- ^ Assertion that the transaction is invalid.
               -> m (TxBody era)                                      -- ^ Action to build the transaction body.
runTransactionImpl connection marloweInBundle marloweOut' inputs outputs changeAddress signingKeys metadata timeout printStats invalid =
  do
    protocol <- queryInEra connection QueryProtocolParameters
    era <- askEra @era
    let
      marloweParams = MC.marloweParams $ mtRolesCurrency marloweOut'
      go :: MarloweTransaction lang era -> m (TxBody era)
      go marloweOut = do

        (spend, collateral, datumOutputs) <-
          case marloweInBundle of
            Nothing                                 -> pure ([], Nothing, [])
            Just (marloweIn, spend, collateral) -> do
                                                        let
                                                          validatorInfo = mtValidator marloweIn
                                                          validator = validatorInfoScriptOrReference validatorInfo

                                                          marloweParams' = MC.marloweParams $ mtRolesCurrency marloweIn
                                                          redeemer = riRedeemer $ buildRedeemer (mtInputs marloweOut)
                                                          inputDatum  = diDatum $ buildMarloweDatum marloweParams (mtContract marloweIn) (mtState marloweIn)
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
                                                        when (marloweParams /= marloweParams') $
                                                          throwError "MarloweParams value is not preserved in continuation"
                                                        pure ([spend'], Just collateral, merkles)
        let
          network = localNodeNetworkId connection

          scriptAddress = viAddress $ mtValidator marloweOut

          outputDatum = diDatum $ buildMarloweDatum marloweParams (mtContract marloweOut) (mtState marloweOut)
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
        (payments :: [(AddressInEra era, Maybe Datum, Api.Value)]) <-
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
                                      (_, money'') <- liftCli $ adjustMinimumUTxO era protocol address Nothing money' ReferenceScriptNone
                                      pure $ Just (address, Nothing, money'')
                Party (Role role) -> do
                                      money' <-
                                        liftCli
                                          $ toCardanoValue money
                                      let
                                        datum = Just . diDatum $ buildRoleDatum (Token (rolesCurrency marloweParams) role)
                                      (_, money'') <- liftCli $ adjustMinimumUTxO era protocol roleAddress datum money' ReferenceScriptNone
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
        outputs' <- for (payments <> outputs <> datumOutputs) (uncurry3 makeTxOut')
        body <-
          buildBody connection
            spend
            continue
            []
            inputs outputs'
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
              -> [SigningKeyFile]                              -- ^ The files for required signing keys.
              -> Maybe FilePath                          -- ^ The file containing JSON metadata, if any.
              -> TxBodyFile                              -- ^ The output file for the transaction body.
              -> Maybe Int                               -- ^ Number of seconds to wait for the transaction to be confirmed, if it is to be confirmed.
              -> Bool                                    -- ^ Whether to print statistics about the transaction.
              -> Bool                                    -- ^ Assertion that the transaction is invalid.
              -> m TxId                                  -- ^ Action to build the transaction body.
withdrawFunds connection marloweOutFile roleName collateral inputs outputs changeAddress signingKeyFiles metadataFile (TxBodyFile bodyFile) timeout printStats invalid =
  do
    metadata <- readMaybeMetadata metadataFile
    SomeMarloweTransaction _ _ marloweOut <- decodeFileStrict marloweOutFile
    let
      rolesCurrency = mtRolesCurrency marloweOut
      roleToken = Token rolesCurrency roleName
    signingKeys <- mapM readSigningKey signingKeyFiles
    roleHash <- liftCli . toCardanoScriptDataHash . diHash $ buildRoleDatum roleToken
    let
      validatorInfo = mtRoleValidator marloweOut
      roleScript = validatorInfoScriptOrReference validatorInfo
      roleAddress = viAddress validatorInfo
      roleDatum = diDatum $ buildRoleDatum roleToken
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
    outputs' <- mapM (uncurry3 makeTxOut') $ withdrawal : outputs
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


-- | Run a Marlowe transaction using FS, without selecting inputs or outputs.
autoRunTransaction :: forall era m
                . MonadError CliError m
               => MonadIO m
               => MonadReader (CliEnv era) m
               => LocalNodeConnectInfo CardanoMode              -- ^ The connection info for the local node.
               -> Maybe (FilePath, TxIn)                        -- ^ The JSON file with the Marlowe initial state and initial contract, along with the script eUTxO being spent and the collateral, unless the transaction opens the contract.
               -> FilePath                                      -- ^ The JSON file with the Marlowe inputs, final state, and final contract.
               -> AddressInEra era                              -- ^ The change address.
               -> [SigningKeyFile]                              -- ^ The files for required signing keys.
               -> Maybe FilePath                                -- ^ The file containing JSON metadata, if any.
               -> TxBodyFile                                    -- ^ The output file for the transaction body.
               -> Maybe Int                                     -- ^ Number of seconds to wait for the transaction to be confirmed, if it is to be confirmed.
               -> Bool                                          -- ^ Whether to print statistics about the transaction.
               -> Bool                                          -- ^ Assertion that the transaction is invalid.
               -> m TxId                                        -- ^ Action to build the transaction body.
autoRunTransaction connection marloweInBundle marloweOutFile changeAddress signingKeyFiles metadataFile (TxBodyFile bodyFile) timeout printStats invalid =
  do
    metadata <- readMaybeMetadata metadataFile
    SomeMarloweTransaction _ era' marloweOut' <- decodeFileStrict marloweOutFile
    era <- askEra @era
    signingKeys <- mapM readSigningKey signingKeyFiles
    let
      go :: forall lang. IsPlutusScriptLanguage lang => C.IsCardanoEra era => MarloweTransaction lang era -> m TxId
      go marloweOut'' = do
        marloweInBundle' <- case marloweInBundle of
          Nothing -> pure Nothing
          Just (marloweInFile, marloweTxIn) -> do
            marloweIn <- readMarloweTransactionFile (plutusScriptVersion :: PlutusScriptVersion lang) marloweInFile
            pure $ Just (marloweIn, marloweTxIn)

        (body :: TxBody era) <- autoRunTransactionImpl connection marloweInBundle' marloweOut'' changeAddress signingKeys metadata timeout printStats invalid
        -- Write the transaction file.
        doWithCardanoEra $ liftCliIO $ writeFileTextEnvelope bodyFile Nothing body
        pure $ getTxId body

    case (era, era') of
      (ScriptDataInAlonzoEra, ScriptDataInAlonzoEra)   -> go marloweOut'
      (ScriptDataInBabbageEra, ScriptDataInBabbageEra) -> go marloweOut'
      (ScriptDataInAlonzoEra, ScriptDataInBabbageEra)  -> throwError "Running in Alonzo era, read file in Babbage era"
      (ScriptDataInBabbageEra, ScriptDataInAlonzoEra)  -> throwError "Running in Babbage era, read file in Alonzo era"


-- | Run a Marlowe transaction, without selecting inputs or outputs.
autoRunTransactionImpl :: forall era lang m
                . MonadError CliError m
               => IsPlutusScriptLanguage lang
               => MonadIO m
               => MonadReader (CliEnv era) m
               => LocalNodeConnectInfo CardanoMode            -- ^ The connection info for the local node.
               -> Maybe (MarloweTransaction lang era, TxIn)   -- ^ The JSON file with the Marlowe initial state and initial contract, unless the transaction opens the contract.
               -> MarloweTransaction lang era                 -- ^ The JSON file with the Marlowe inputs, final state, and final contract.
               -> AddressInEra era                            -- ^ The change address.
               -> [SomePaymentSigningKey]
               -> TxMetadataInEra era                         -- ^ The file containing JSON metadata, if any.
               -> Maybe Int                                   -- ^ Number of seconds to wait for the transaction to be confirmed, if it is to be confirmed.
               -> Bool                                        -- ^ Whether to print statistics about the transaction.
               -> Bool                                        -- ^ Assertion that the transaction is invalid.
               -> m (TxBody era)                              -- ^ Action to build the transaction body.
autoRunTransactionImpl connection marloweInBundle marloweOut' changeAddress signingKeys metadata timeout printStats invalid =
  do
    -- Fetch the protocol parameters.
    protocol <- queryInEra connection QueryProtocolParameters
    -- Read the Marlowe transaction information for the output.
    -- Fetch the era.
    era <- askEra @era
    let
      go :: MarloweTransaction lang era -> m (TxBody era)
      go marloweOut = do
        let
          rolesCurrency = mtRolesCurrency marloweOut
          marloweParams = MC.marloweParams $ mtRolesCurrency marloweOut
        -- Find the script UTxO to spend and the datum for outputs to the role payout address.
        (spend, datumOutputs) <-
          case marloweInBundle of
                                           -- This is a creation transaction.
            Nothing                     -> pure ([], [])
                                           -- This is a non-creation transaction.
            Just (marloweIn, spend) -> do
                                            -- Find the results of the previous transaction.
                                            -- SomeMarloweTransaction _ _ marloweIn  <- decodeFileStrict marloweInFile
                                            let
                                              -- Fetch the validator.
                                              validatorInfo = mtValidator marloweIn
                                              validator = validatorInfoScriptOrReference validatorInfo
                                              -- Build the redeemer.
                                              redeemer = riRedeemer $ buildRedeemer (mtInputs marloweOut)
                                              -- Build the datum.
                                              inputDatum  = diDatum $ buildMarloweDatum marloweParams (mtContract marloweIn) (mtState marloweIn)
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
          outputDatum = diDatum $ buildMarloweDatum marloweParams (mtContract marloweOut) (mtState marloweOut)
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
                                        datum = Just . diDatum $ buildRoleDatum (Token rolesCurrency role)
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
                value <- liftCli . toCardanoValue $ singleton (mtRolesCurrency marloweOut) role 1
                ensureMinUtxo protocol (changeAddress, Nothing, value)
            |
              role <- roles $ mtInputs marloweOut
            ]
        txOuts <- traverse
          (uncurry3 makeTxOut')
          (payments <> roleOutputs <> datumOutputs)

        -- Select the coins.
        (collateral, extraInputs, revisedOutputs)
          <- selectCoins
               connection
               incoming
               txOuts
               continue
               changeAddress
               defaultCoinSelectionStrategy
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
        -- Optionally submit the transaction, waiting for a timeout.
        forM_ timeout
          $ if invalid
              then const $ throwError "Refusing to submit an invalid transaction: collateral would be lost."
              else submitBody connection body signingKeys
        -- Return the transaction identifier.
        pure body
    go marloweOut'


-- | Withdraw funds for a specific role from the role address, without selecting inputs or outputs.
autoWithdrawFunds :: forall era m
                   . MonadError CliError m
                  => MonadReader (CliEnv era) m
                  => MonadIO m
                  => C.IsCardanoEra era
                  => LocalNodeConnectInfo CardanoMode        -- ^ The connection info for the local node.
                  -> FilePath                                -- ^ The JSON file with the Marlowe state and contract.
                  -> TokenName                               -- ^ The role name for the redemption.
                  -> AddressInEra era                        -- ^ The change address.
                  -> [SigningKeyFile]                        -- ^ The files for required signing keys.
                  -> Maybe FilePath                          -- ^ The file containing JSON metadata, if any.
                  -> TxBodyFile                              -- ^ The output file for the transaction body.
                  -> Maybe Int                               -- ^ Number of seconds to wait for the transaction to be confirmed, if it is to be confirmed.
                  -> PrintStats                              -- ^ Whether to print statistics about the transaction.
                  -> Bool                                    -- ^ Assertion that the transaction is invalid.
                  -> m TxId                                  -- ^ Action to build the transaction body.
autoWithdrawFunds connection marloweOutFile roleName changeAddress signingKeyFiles metadataFile bodyFile timeout printStats invalid =
  do
    era <- askEra
    -- Read the Marlowe transaction information that was used to populate the role-payout address.
    SomeMarloweTransaction _ era' marloweOut <- decodeFileStrict marloweOutFile
    let
      go :: forall lang. IsPlutusScriptLanguage lang => MarloweTransaction lang era -> m TxId
      go marloweOut' = do
        -- Read the signing keys.
        signingKeys <- mapM readSigningKey signingKeyFiles
        -- Read any metadata for the transaction.
        metadata <- readMaybeMetadata metadataFile
        -- Write the transaction file.
        (txBody :: TxBody era) <- autoWithdrawFundsImpl connection marloweOut' roleName changeAddress signingKeys metadata timeout printStats invalid

        doWithCardanoEra $ liftCliIO $ writeFileTextEnvelope (unTxBodyFile bodyFile) Nothing txBody
        pure $ getTxId txBody

    case (era, era') of
      (ScriptDataInAlonzoEra, ScriptDataInAlonzoEra)   -> go marloweOut
      (ScriptDataInBabbageEra, ScriptDataInBabbageEra) -> go marloweOut
      (ScriptDataInAlonzoEra, ScriptDataInBabbageEra)  -> throwError "Running in Alonzo era, read file in Babbage era"
      (ScriptDataInBabbageEra, ScriptDataInAlonzoEra)  -> throwError "Running in Babbage era, read file in Alonzo era"


-- | Withdraw funds for a specific role from the role address, without selecting inputs or outputs.
autoWithdrawFundsImpl :: forall era lang m
                   . MonadError CliError m
                  => MonadReader (CliEnv era) m
                  => MonadIO m
                  => IsPlutusScriptLanguage lang
                  => LocalNodeConnectInfo CardanoMode        -- ^ The connection info for the local node.
                  -> MarloweTransaction lang era             -- ^ The JSON file with the Marlowe state and contract.
                  -> TokenName                               -- ^ The role name for the redemption.
                  -> AddressInEra era                        -- ^ The change address.
                  -> [SomePaymentSigningKey]                 -- ^ Required signing keys.
                  -> TxMetadataInEra era                     -- ^ The file containing JSON metadata, if any.
                  -> Maybe Int                               -- ^ Number of seconds to wait for the transaction to be confirmed, if it is to be confirmed.
                  -> PrintStats                              -- ^ Whether to print statistics about the transaction.
                  -> Bool                                    -- ^ Assertion that the transaction is invalid.
                  -> m (TxBody era)                          -- ^ Action to build the transaction body.
autoWithdrawFundsImpl connection marloweOut roleName changeAddress signingKeys metadata timeout (PrintStats printStats) invalid =
  do
    -- Fetch the protocol parameters.
    protocol <- queryInEra connection QueryProtocolParameters
    let
      roleCurrency = mtRolesCurrency marloweOut
      -- Build the datum corresponding to the role name.
      roleDatum = buildRoleDatum (Token roleCurrency roleName)
    -- Compute the hash of the role name.
    roleHash <- liftCli . toCardanoScriptDataHash . diHash $ roleDatum
    let
      -- Compute the validator information.
      validatorInfo = mtRoleValidator marloweOut
      -- Fetch the role-payout validator script.
      roleScript = validatorInfoScriptOrReference validatorInfo
      -- Fetch the role address.
      roleAddress = viAddress validatorInfo
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
    role <- liftCli $ toCardanoValue $ singleton (mtRolesCurrency marloweOut) roleName 1
    let
      -- Build the spending from the script.
      spend = buildPayFromScript roleScript (diDatum roleDatum) roleRedeemer . fst <$> utxos
      -- Find how much is being spent from the script.
      -- The output value should include the spending from the script and the role token.
      withdrawn = mconcat [txOutValueToValue value | (_, TxOut _ value _ _) <- utxos]
    -- Ensure that the output meets the min-Ada requirement.
    output <- ensureMinUtxo protocol (changeAddress, Nothing, withdrawn <> role) >>= uncurry3 makeTxOut'
    -- Select the coins.
    (collateral, extraInputs, revisedOutputs) <-
      selectCoins
        connection
        withdrawn
        [output]
        Nothing
        changeAddress
        defaultCoinSelectionStrategy
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
    -- Optionally submit the transaction, waiting for a timeout.
    forM_ timeout
      $ if invalid
          then const $ throwError "Refusing to submit an invalid transaction: collateral would be lost."
          else submitBody connection body signingKeys
    -- Return the transaction identifier.
    pure body
