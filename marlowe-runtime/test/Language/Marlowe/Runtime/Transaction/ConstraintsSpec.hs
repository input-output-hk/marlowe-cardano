{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}

module Language.Marlowe.Runtime.Transaction.ConstraintsSpec
  where

import Cardano.Api
import Cardano.Api.Shelley
  (PlutusScriptOrReferenceInput(PScript), ProtocolParameters(..), ReferenceScript(ReferenceScriptNone))
import Control.Applicative (Alternative)
import Control.Arrow ((***))
import Control.Error (note)
import Control.Monad (guard)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Either (fromLeft, isRight)
import Data.Foldable (fold)
import Data.Functor ((<&>))
import Data.List (find, isPrefixOf)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Map.Strict as SMap (toList)
import Data.Maybe (fromJust, isJust, mapMaybe)
import Data.Monoid (First(..), getFirst)
import Data.Ratio ((%))
import qualified Data.Set as Set
import Data.Traversable (for)
import Data.Word (Word32)
import GHC.Word (Word64)
import Gen.Cardano.Api.Metadata (genTxMetadataValue)
import Gen.Cardano.Api.Typed
  ( genAddressByron
  , genAddressShelley
  , genPlutusScript
  , genProtocolParameters
  , genScriptData
  , genScriptHash
  , genTxBodyContent
  , genValueForTxOut
  , genVerificationKey
  )
import Language.Marlowe (MarloweData(..), MarloweParams(..), txInputs)
import qualified Language.Marlowe.Core.V1.Semantics.Types as V1
import Language.Marlowe.Runtime.Cardano.Api
import qualified Language.Marlowe.Runtime.ChainSync.Api as Chain
import Language.Marlowe.Runtime.Core.Api
  ( Contract
  , Datum
  , Inputs
  , MarloweVersion(..)
  , MarloweVersionTag(..)
  , Payout(..)
  , PayoutDatum
  , TransactionScriptOutput(..)
  , toChainDatum
  , toChainPayoutDatum
  )
import Language.Marlowe.Runtime.Core.ScriptRegistry (ReferenceScriptUtxo(..))
import Language.Marlowe.Runtime.Transaction.Constraints
import qualified Language.Marlowe.Scripts as V1
import Spec.Marlowe.Common (shrinkContract)
import Spec.Marlowe.Semantics.Arbitrary (SemiArbitrary(semiArbitrary), arbitraryValidInput)
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck hiding (shrinkMap)
import Test.QuickCheck.Hedgehog (hedgehog)
import Text.Printf (printf)

spec :: Spec
spec = do
  describe "solveInitialTxBodyContent" do
    prop "satisfies the constraints" \(SomeTxConstraints marloweVersion constraints) -> do
      protocol <- hedgehog genProtocolParameters
      marloweContext <- genMarloweContext marloweVersion constraints
      walletContext <- genWalletContext marloweVersion constraints
      let
        (marloweContextStr, walletContextStr) = case marloweVersion of MarloweV1 -> (show marloweContext, show walletContext)
        marloweUtxo = case scriptOutput marloweContext of
          Nothing -> mempty
          Just TransactionScriptOutput{..} -> Chain.UTxOs
            $ Map.singleton utxo
            $ Chain.TransactionOutput address assets Nothing (Just $ toChainDatum marloweVersion datum)
        payoutToTransactionOutput Payout{..} = Chain.TransactionOutput address assets Nothing (Just $ toChainPayoutDatum marloweVersion datum)
        payoutUtxos = Chain.UTxOs $ payoutToTransactionOutput <$> payoutOutputs marloweContext
        referenceScriptUtxoToUtxo ReferenceScriptUtxo{..} = (txOutRef, txOut)
        referenceUtxos = Chain.UTxOs $ Map.fromList $ referenceScriptUtxoToUtxo <$> [marloweScriptUTxO marloweContext, payoutScriptUTxO marloweContext]
        utxosFromMarloweContext = marloweUtxo <> payoutUtxos <> referenceUtxos
        utxos = utxosFromMarloweContext <> availableUtxos walletContext
        result = solveInitialTxBodyContent protocol marloweVersion marloweContext walletContext constraints
        mViolations = violations marloweVersion marloweContext utxos constraints <$> result
        theProperty :: Property
        theProperty = case marloweVersion of
          MarloweV1 -> Right [] === mViolations
      pure
        $ counterexample marloweContextStr
        $ counterexample walletContextStr
        $ counterexample (show utxos)
        $ either (const theProperty) (flip counterexample theProperty . show) result

  describe "adjustTxForMinUtxo" do
    prop "Marlowe output is NOT adjusted" do
      marloweScriptHash <- hedgehog genScriptHash
      let
        marloweAddressCardano = AddressInEra (ShelleyAddressInEra ShelleyBasedEraBabbage)
          $ makeShelleyAddress Mainnet (PaymentCredentialByScript marloweScriptHash) NoStakeAddress
        marloweAddressChain = fromCardanoAddressInEra BabbageEra marloweAddressCardano

        getValueAtAddress :: Chain.Address -> [TxOut CtxTx BabbageEra] -> Maybe (TxOutValue BabbageEra)
        getValueAtAddress targetAddress txOuts = getFirst . mconcat . map (First
          . (\(TxOut addressInEra txOutValue _ _) ->
              if fromCardanoAddressInEra BabbageEra addressInEra == targetAddress
                then Just txOutValue
                else Nothing))
          $ txOuts

      txBodyContent <- do
        txBC <- hedgehog $ genTxBodyContent BabbageEra
        lovelaceAmount <- (2_000_000 +) <$> suchThat arbitrary (> 0)
        pure $ txBC { txOuts =
          [ TxOut
              marloweAddressCardano
              (lovelaceToTxOutValue $ Lovelace lovelaceAmount)
              TxOutDatumNone
              ReferenceScriptNone
          ] }

      let
        actual = getValueAtAddress marloweAddressChain . txOuts
          <$> adjustTxForMinUtxo protocolTestnet marloweAddressChain txBodyContent
        expected :: Either (ConstraintError 'V1) (Maybe (TxOutValue BabbageEra))
          = Right $ getValueAtAddress marloweAddressChain $ txOuts txBodyContent

      pure $ actual `shouldBe` expected

    prop "all outputs satisfy Cardano API minimum UTxO requirements" do
      marloweScriptHash <- hedgehog genScriptHash

      let
        scriptAddress hash = fromCardanoAddressAny
          $ AddressShelley
          $ makeShelleyAddress Mainnet (PaymentCredentialByScript hash) NoStakeAddress
        marloweAddress = scriptAddress marloweScriptHash

        valueMeetsMinimumReq :: TxOut CtxTx BabbageEra -> Maybe String
        valueMeetsMinimumReq txOut@(TxOut _ txOrigValue _ _) =
          case calculateMinimumUTxO ShelleyBasedEraBabbage txOut protocolTestnet of
            Right minValueFromApi ->
              if origAda >= selectLovelace minValueFromApi
                then Nothing
                else Just $ printf "Value %s is lower than minimum value %s"
                      (show origAda) (show $ selectLovelace minValueFromApi)
            Left exception -> Just $ show exception
            where
              origAda = selectLovelace . txOutValueToValue $ txOrigValue

      txBodyContent <- hedgehog $ genTxBodyContent BabbageEra

      pure $ case adjustTxForMinUtxo protocolTestnet marloweAddress txBodyContent of
        Right newTxBodyContent -> do
          let errors = mapMaybe valueMeetsMinimumReq $ txOuts newTxBodyContent
          if null errors
            then pure ()
            else expectationFailure $ unlines $ "Minimum UTxO requirements not met:" : errors
        Left (msgFromAdjustment :: ConstraintError 'V1) -> expectationFailure $ show msgFromAdjustment

    prop "all outputs are at least half an ADA" do
      marloweScriptHash <- hedgehog genScriptHash

      let
        scriptAddress hash = fromCardanoAddressAny
          $ AddressShelley
          $ makeShelleyAddress Mainnet (PaymentCredentialByScript hash) NoStakeAddress
        marloweAddress = scriptAddress marloweScriptHash

        valueIsAtLeastHalfAnAda :: TxOut CtxTx BabbageEra -> Maybe String
        valueIsAtLeastHalfAnAda (TxOut _ txOrigValue _ _) =
          if origAda >= Lovelace 500_000
            then Nothing
            else Just $ printf "An output is %s but should be at least 500_000 Lovelace" (show origAda)
          where origAda = selectLovelace . txOutValueToValue $ txOrigValue

      txBodyContent <- hedgehog $ genTxBodyContent BabbageEra

      pure $ case adjustTxForMinUtxo protocolTestnet marloweAddress txBodyContent of
        Right newTxBodyContent -> do
          let errors = mapMaybe valueIsAtLeastHalfAnAda $ txOuts newTxBodyContent
          if null errors
            then pure ()
            else expectationFailure $ unlines $ "Minimum UTxO requirements not met:" : errors
        Left (msgFromAdjustment :: ConstraintError 'V1) -> expectationFailure $ show msgFromAdjustment

  describe "selectCoins" do
    prop "sufficient collateral is selected if possible" \(SomeTxConstraints marloweVersion constraints) -> do
      -- This test is broadly doing this:
      -- - Start with an empty tx body
      -- - and an empty (as possible) Marlowe context
      -- - In an empty wallet context (we can use arbitrary instance to generate utxos to spend)
      -- - Perform coin selection
      -- - Only look at collateral
      -- - Looking for a pure ADA utxo that's 2x the fee (protocol maximum fee)
      -- - If it's selecting collat that has a native token, that's failure, not supposed to do that

      marloweContext <- genSimpleMarloweContext marloweVersion constraints

      -- We MUST dictate the distribution of wallet context assets, default
      -- generation only tests with empty wallets!
      maxLovelace <- choose (0, 40_000_000)
      walletContext <- genWalletWithAsset marloweVersion constraints maxLovelace

      let
        extractCollat :: TxBodyContent BuildTx BabbageEra -> [Chain.Assets]
        extractCollat txBC = map Chain.assets selectedCollat
          where
            -- Extract the [(TxOutRef, TransactionOutput)] from the walletContext
            utT = map Chain.toUTxOTuple . Chain.toUTxOsList . availableUtxos $ walletContext

            -- True if the Chain.TxOutRef refers to the same tx as the (Cardano) TxIn
            txInEqTxOutRef :: Chain.TxOutRef -> TxIn -> Bool
            txInEqTxOutRef (Chain.TxOutRef txIdX txIxX) (TxIn txIdY txIxY) =
              (txIdX == fromCardanoTxId txIdY) && (txIxX == fromCardanoTxIx txIxY)

            -- Turn the TxInsCollateral ADT into a simple list
            insCollatToTxInList :: TxInsCollateral BabbageEra -> [TxIn]
            insCollatToTxInList TxInsCollateralNone = []
            insCollatToTxInList (TxInsCollateral _ txIns) = txIns

            -- The list of TxIn in the TxBodyContent
            selectedTxIns :: [TxIn] = insCollatToTxInList . txInsCollateral $ txBC

            -- Filter the utT :: [(TxOutRef, TransactionOutput)] for all that
            -- have a member in wantedTxIns [TxIn]
            selectedCollat :: [Chain.TransactionOutput]
            selectedCollat = map snd
              . filter (\(txOutRef, _) -> any (txInEqTxOutRef txOutRef) selectedTxIns) $ utT

        -- Convert chain UTxOs to Cardano API ones.
        convertUtxo :: (Chain.TxOutRef, Chain.TransactionOutput) -> Maybe (TxOut ctx BabbageEra)
        convertUtxo (_, transactionOutput) =
          toCardanoTxOut' MultiAssetInBabbageEra transactionOutput Nothing

        -- All utxos that are spendable from the wallet context
        utxos :: [TxOut CtxTx BabbageEra]
        utxos = mapMaybe convertUtxo (SMap.toList . Chain.unUTxOs . availableUtxos $ walletContext)

        -- Compute the value of all available UTxOs
        universe :: Value
        universe = foldMap txOutToValue utxos

        chAddress = changeAddressFromWallet walletContext

        minUtxo = findMinUtxo protocolTestnet chAddress universe
               <> findMinUtxo protocolTestnet chAddress mempty

        --walletContext has sufficient collateral ADA
        walletCtxSufficient :: Bool
        walletCtxSufficient = allAdaOnly && anyCoversFee
          where
            allAssets = map (Chain.assets . snd . Chain.toUTxOTuple)
              . Chain.toUTxOsList . availableUtxos $ walletContext
            onlyAdas = filter (isRight . assetsAdaOnly) allAssets
            allAdaOnly = not . null $ onlyAdas
            fee' = maxFee protocolTestnet
            minUtxo' = selectLovelace minUtxo
            targetLovelace = fromCardanoLovelace $ fee' <> minUtxo'
            anyCoversFee = any ((>= targetLovelace) . Chain.ada) onlyAdas

        singleUtxo :: [Chain.Assets] -> Either String Chain.Assets
        singleUtxo [as] = Right as
        singleUtxo l = Left $ "Collateral is not exactly one utxo" <> show l

        assetsAdaOnly :: Chain.Assets -> Either String Chain.Lovelace
        assetsAdaOnly as@(Chain.Assets lovelace tokens)
          | Map.null . Chain.unTokens $ tokens = Right lovelace
          | otherwise = Left $ "Collateral contains non-ADA token(s)" <> show as

        adaCollatIsSufficient :: Chain.Lovelace -> Either String ()
        adaCollatIsSufficient lovelace
          | lovelace >= (fromCardanoLovelace $ maxFee protocolTestnet) = Right ()
          | otherwise = Left $ "Collateral doesn't cover the fees. collat: "
              <> show lovelace <> "  fees: " <> show (maxFee protocolTestnet)

        -- Function to convert the Left side of the Either from (ConstraintError v) to String
        selectResult :: Either String ()
        selectResult = either
          (\ce -> case marloweVersion of MarloweV1 -> Left . show $ ce)
          (\txBC -> singleUtxo (extractCollat txBC) >>= assetsAdaOnly >>= adaCollatIsSufficient)
          $ selectCoins protocolTestnet marloweVersion marloweContext
              walletContext emptyTxBodyContent

      pure $ case (walletCtxSufficient, selectResult) of
        (True , Right _) -> label "Wallet has funds, selection succeeded" True
        (False, Right _) -> counterexample "Selection should have failed" False
        (True , Left selFailedMsg) ->
          counterexample ("Selection shouldn't have failed\n" <> selFailedMsg) False
        (False, Left selFailedMsg) -> label "Wallet does not have funds, selection failed"
          $ selFailedMsg `shouldSatisfy` isPrefixOf "CoinSelectionFailed"

    prop "selectCoins should increase the number of outputs by either 0 or exactly 1" \(SomeTxConstraints marloweVersion constraints) -> do
      marloweContext <- genSimpleMarloweContext marloweVersion constraints
      walletContext <- genWalletWithNuisance marloweVersion constraints 1_000_000_000
      txBodyContentBefore <- genBodyContentWith500AdaOutput

      let
        -- Get a non-ADA asset count from a wallet context
        nonAdaCountWalletCtx :: WalletContext -> Int
        nonAdaCountWalletCtx = length . filter (not . Map.null)
          . map (Chain.unTokens . Chain.tokens . Chain.assets . Chain.transactionOutput)
          . Chain.toUTxOsList . availableUtxos

        -- Function to convert the Left side of the Either from (ConstraintError v) to String
        selectResult :: Either String (TxBodyContent BuildTx BabbageEra)
        selectResult = either
          (\ce -> case marloweVersion of MarloweV1 -> Left . show $ ce)
          Right
          $ selectCoins protocolTestnet marloweVersion marloweContext
              walletContext txBodyContentBefore

      pure $ case selectResult of
        Right txBodyContentAfter -> do
          let
            txOutsBefore = length . txOuts $ txBodyContentBefore
            txOutsAfter = length . txOuts $ txBodyContentAfter
            walletCt = nonAdaCountWalletCtx walletContext
          label (printf "outputs: wallet %d, tx before %d, tx after %d" walletCt txOutsBefore txOutsAfter)
            $ txOutsAfter `shouldBe` (txOutsBefore + walletCt)
        Left selFailedMsg -> counterexample ("selection failed: " <> selFailedMsg) False

    prop "selectCoins creates a balanceable tx" \(SomeTxConstraints marloweVersion constraints) -> do
      marloweContext <- genSimpleMarloweContext marloweVersion constraints
      walletContext <- genWalletWithNuisance marloweVersion constraints 1_000_000_000
      txBodyContentBefore <- genBodyContentWith500AdaOutput

      let
        walletHasValue :: WalletContext -> WalletValueDesc
        walletHasValue wc = case (hasAda, hasNonAda) of
          (False, False) -> EmptyWallet
          (True , False) -> WalletHasOnlyAda
          (False, True ) -> WalletHasOnlyNonAda
          (True , True ) -> WalletHasBoth
          where
            allAssets = map (Chain.assets . snd) . Map.toList . Chain.unUTxOs . availableUtxos $ wc
            hasAda = any ((/= 0) . Chain.unLovelace . Chain.ada) allAssets
            hasNonAda = not (all (Map.null . Chain.unTokens . Chain.tokens) allAssets)

        -- There are two sets of TxIns in a TxBodyContent. "normal" inputs and
        -- collateral inputs. These two functions get those as a simple [TxIn]
        -- which is the common type needed by txInsToValue below.

        txInsFromTxBodyIns :: TxBodyContent BuildTx BabbageEra -> [TxIn]
        txInsFromTxBodyIns = map fst . txIns

        -- Unused at this time, but maybe someday?
        -- txInsFromTxBodyCollat :: TxBodyContent BuildTx BabbageEra -> [TxIn]
        -- txInsFromTxBodyCollat txbc = case txInsCollateral txbc of
        --   TxInsCollateralNone -> []
        --   TxInsCollateral _ txIns -> txIns

        txInsToValue :: [TxIn] -> Value
        txInsToValue txIns = mconcat . map (assetsToValue . Chain.assets) $ selectedOutputs
          where
            -- Extract the [(TxOutRef, TransactionOutput)] from the walletContext
            utT = map Chain.toUTxOTuple . Chain.toUTxOsList . availableUtxos $ walletContext

            -- True if the Chain.TxOutRef refers to the same tx as the (Cardano) TxIn
            txInEqTxOutRef :: Chain.TxOutRef -> TxIn -> Bool
            txInEqTxOutRef (Chain.TxOutRef txIdX txIxX) (TxIn txIdY txIxY) =
              (txIdX == fromCardanoTxId txIdY) && (txIxX == fromCardanoTxIx txIxY)

            -- Filter the utT :: [(TxOutRef, TransactionOutput)] for all that
            -- have a member in wantedTxIns [TxIn]
            selectedOutputs :: [Chain.TransactionOutput]
            selectedOutputs = map snd
              . filter (\(txOutRef, _) -> any (txInEqTxOutRef txOutRef) txIns) $ utT

        fromChainTokens :: Chain.Tokens -> [(AssetId, Quantity)]
        fromChainTokens (Chain.Tokens chainTokenMap) =
          map (fromChainAssetId *** fromChainQuantity)
          . Map.toList
          $ chainTokenMap
          where
            fromChainAssetId :: Chain.AssetId -> AssetId
            fromChainAssetId (Chain.AssetId chPolId (Chain.TokenName tokNameBS)) =
              AssetId (fromJust $ toCardanoPolicyId chPolId) (AssetName tokNameBS)

            fromChainQuantity :: Chain.Quantity -> Quantity
            fromChainQuantity (Chain.Quantity i) = Quantity (fromIntegral i)

        assetsToValue :: Chain.Assets -> Value
        assetsToValue (Chain.Assets (Chain.Lovelace 0) chTokens) =
          valueFromList $ fromChainTokens chTokens
        assetsToValue (Chain.Assets (Chain.Lovelace l) chTokens) =
          valueFromList $ (AdaAssetId, Quantity (fromIntegral l)) : fromChainTokens chTokens

        txOutsToValue :: TxBodyContent BuildTx BabbageEra -> Value
        txOutsToValue = mconcat . map txOutToValue . txOuts

        -- Function to convert the Left side of the Either from (ConstraintError v) to String
        selectResult :: Either String (TxBodyContent BuildTx BabbageEra)
        selectResult = either
          (\ce -> case marloweVersion of MarloweV1 -> Left . show $ ce)
          Right
          $ selectCoins protocolTestnet marloweVersion marloweContext
              walletContext txBodyContentBefore

      pure $ case selectResult of
        Right txBodyContentAfter -> do
          label (show . walletHasValue $ walletContext) $ do
            let
              valOuts = txOutsToValue txBodyContentAfter
              valIns = txInsToValue . txInsFromTxBodyIns $ txBodyContentAfter

              adaIns = lovelaceToValue . selectLovelace $ valIns
              tokIns = filterValue (/= AdaAssetId) valIns

              adaOuts = lovelaceToValue . selectLovelace $ valOuts
              tokOuts = filterValue (/= AdaAssetId) valOuts

              minUtxo = selectLovelace $ findMinUtxo protocolTestnet
                (changeAddressFromWallet walletContext) mempty
              maxFee' = maxFee protocolTestnet

            -- check 1: pure ADA inputs - pure ADA outputs >= min utxo + max fee
            selectLovelace (adaIns <> negateValue adaOuts) >= minUtxo <> maxFee' `shouldBe` True
            -- check 2: non-ADA inputs and non-ADA outputs cancel each other out
            tokIns <> negateValue tokOuts `shouldBe` mempty
        Left selFailedMsg -> counterexample ("selection failed: " <> selFailedMsg) False

-- Generate a wallet that always has a pure ADA value of 7 and a value
-- with a minimum ADA plus zero or more "nuisance" tokens
genWalletWithNuisance :: MarloweVersion v -> TxConstraints v -> Word64 -> Gen WalletContext
genWalletWithNuisance marloweVersion' constraints' minLovelace = do
  wc <- genWalletContext marloweVersion' constraints'
  adaTxOutRef <- genTxOutRef
  nuisTxOutRef <- genTxOutRef
  someAddress <- genAddress
  let lovelaceToAdd = Chain.Assets (Chain.Lovelace minLovelace) (Chain.Tokens Map.empty)
  nuisAssets <- (lovelaceToAdd <>) <$> genOutAssets
  let
    adaAssets = Chain.Assets (Chain.Lovelace 7_000_000) (Chain.Tokens Map.empty)
    adaTxOut = Chain.TransactionOutput someAddress adaAssets Nothing Nothing
    nuisTxOut = Chain.TransactionOutput someAddress nuisAssets Nothing Nothing
    utxos = Chain.UTxOs $ Map.fromList [(adaTxOutRef, adaTxOut), (nuisTxOutRef, nuisTxOut)]
  pure $ wc { availableUtxos = utxos, collateralUtxos = Set.singleton adaTxOutRef }

-- Simulate constraints specifying the tx must cover a 500ADA output
-- after coin selection. This exists to force selection to consume the
-- input(s) in the wallet.
genBodyContentWith500AdaOutput :: Gen (TxBodyContent BuildTx BabbageEra)
genBodyContentWith500AdaOutput = do
  addr <- hedgehog $ AddressInEra (ShelleyAddressInEra ShelleyBasedEraBabbage) <$> genAddressShelley
  pure $ emptyTxBodyContent { txOuts =
    [ TxOut
        addr
        (lovelaceToTxOutValue $ Lovelace 500_000_000)
        TxOutDatumNone
        ReferenceScriptNone
    ] }

maxFee :: ProtocolParameters -> Lovelace
maxFee ProtocolParameters{..} = 2 * (txFee + round executionFee)
  where
    txFee :: Lovelace
    txFee = fromIntegral $ protocolParamTxFeeFixed + protocolParamTxFeePerByte
      * protocolParamMaxTxSize

    executionFee :: Rational
    executionFee =
      case (protocolParamPrices, protocolParamMaxTxExUnits) of
        (Just ExecutionUnitPrices{..}, Just ExecutionUnits{..})
          -> priceExecutionSteps * fromIntegral executionSteps
              + priceExecutionMemory * fromIntegral executionMemory
        _ -> 0

changeAddressFromWallet :: WalletContext -> AddressInEra BabbageEra
changeAddressFromWallet = anyAddressInShelleyBasedEra . fromJust . Chain.toCardanoAddress . changeAddress

findMinUtxo :: ProtocolParameters -> AddressInEra BabbageEra -> Value -> Value
findMinUtxo protocol chAddress origValue = do
  let
    atLeastHalfAnAda :: Value
    atLeastHalfAnAda = lovelaceToValue (maximum [500_000, selectLovelace origValue])

    dummyTxOut = TxOut
      chAddress (TxOutValue MultiAssetInBabbageEra atLeastHalfAnAda)
      TxOutDatumNone ReferenceScriptNone

  case calculateMinimumUTxO ShelleyBasedEraBabbage dummyTxOut protocol of
     Right minValue -> minValue
     Left _ -> undefined

data WalletValueDesc = EmptyWallet | WalletHasOnlyAda | WalletHasOnlyNonAda | WalletHasBoth
  deriving Show

-- Extract the value of a UTxO
txOutToValue :: TxOut CtxTx BabbageEra -> Value
txOutToValue (TxOut _ value _ _) = txOutValueToValue value

-- A simple Marlowe context with no assets to spend
genSimpleMarloweContext :: MarloweVersion v -> TxConstraints v -> Gen (MarloweContext w)
genSimpleMarloweContext marloweVersion constraints = do
  -- Let the generator make us one..
  mctx <- genMarloweContext marloweVersion constraints
  -- ..and hack these values to be empty/nothing
  pure $ mctx
    { scriptOutput = Nothing
    , payoutOutputs = Map.empty
    }

-- Generate a random amount and add the specified amount of Lovelace to it
genAdaOnlyAssets :: Integer -> Chain.Assets
genAdaOnlyAssets maxLovelace = Chain.Assets
  (fromCardanoLovelace $ Lovelace maxLovelace)
  (Chain.Tokens Map.empty)

-- The simplest wallet context:
--   availableUtxos = A single ADA-only Utxo
--   collateralUtxos = A set containing the one Utxo from above
--   changeAddress = any valid address
genWalletWithAsset :: MarloweVersion v -> TxConstraints v -> Integer -> Gen WalletContext
genWalletWithAsset marloweVersion constraints minLovelace = do
  wc <- genWalletContext marloweVersion constraints
  txOutRef <- genTxOutRef
  stubAddress <- genAddress
  let
    assets = genAdaOnlyAssets minLovelace
    txOut = Chain.TransactionOutput stubAddress assets Nothing Nothing
    utxos = Chain.UTxOs $ Map.singleton txOutRef txOut
  pure $ wc { availableUtxos = utxos, collateralUtxos = Set.singleton txOutRef }

-- A simple TxBodyContent that's completely empty
emptyTxBodyContent :: TxBodyContent BuildTx BabbageEra
emptyTxBodyContent = TxBodyContent
  { txIns = []
  , txInsCollateral = TxInsCollateralNone
  , txInsReference = TxInsReferenceNone
  , txOuts = []
  , txTotalCollateral = TxTotalCollateralNone
  , txReturnCollateral = TxReturnCollateralNone
  , txFee = TxFeeExplicit TxFeesExplicitInBabbageEra 0
  , txValidityRange =
      ( TxValidityNoLowerBound
      , TxValidityNoUpperBound ValidityNoUpperBoundInBabbageEra
      )
  , txMetadata = TxMetadataNone
  , txAuxScripts = TxAuxScriptsNone
  , txExtraKeyWits = TxExtraKeyWitnessesNone
  , txProtocolParams = BuildTxWith $ Just protocolTestnet
  , txWithdrawals = TxWithdrawalsNone
  , txCertificates = TxCertificatesNone
  , txUpdateProposal = TxUpdateProposalNone
  , txMintValue = TxMintNone
  , txScriptValidity = TxScriptValidityNone
  }

violations :: MarloweVersion v -> MarloweContext v -> Chain.UTxOs -> TxConstraints v -> TxBodyContent BuildTx BabbageEra -> [String]
violations marloweVersion marloweContext utxos constraints txBodyContent = fold
  [ ("mustMintRoleToken: " <>) <$> mustMintRoleTokenViolations marloweVersion constraints txBodyContent
  , ("mustSpendRoleToken: " <>) <$> mustSpendRoleTokenViolations marloweVersion utxos constraints txBodyContent
  , ("mustPayToAddress: " <>) <$> mustPayToAddressViolations marloweVersion constraints txBodyContent
  , ("mustSendMarloweOutput: " <>) <$> mustSendMarloweOutputViolations marloweVersion marloweContext constraints txBodyContent
  , ("mustPayToRole: " <>) <$> mustPayToRoleViolations marloweVersion marloweContext constraints txBodyContent
  , ("mustConsumeMarloweOutput: " <>) <$> mustConsumeMarloweOutputViolations marloweVersion marloweContext constraints txBodyContent
  , ("mustConsumePayouts: " <>) <$> mustConsumePayoutsViolations marloweVersion marloweContext constraints txBodyContent
  , ("requiresSignature: " <>) <$> requiresSignatureViolations marloweVersion utxos constraints txBodyContent
  , ("requiresMetadata: " <>) <$> requiresMetadataViolations marloweVersion constraints txBodyContent
  ]

check :: Alternative m => Bool -> a -> m a
check condition msg = msg <$ guard (not condition)

mustMintRoleTokenViolations
  :: MarloweVersion v -> TxConstraints v -> TxBodyContent BuildTx BabbageEra -> [String]
mustMintRoleTokenViolations MarloweV1 TxConstraints{..} TxBodyContent{..} = fold
  [ mintsOneToken
  , sendsOneTokenAndOnlyToken
  , consumesUtxo
  ]
  where
    consumesUtxo = case roleTokenConstraints of
      MintRoleTokens txOutRef _ _ -> do
        check
          (any ((== txOutRef) . fromCardanoTxIn . fst) txIns)
          ("UTxO not consumed: " <> show (Chain.renderTxOutRef txOutRef))
      _ -> []

    sendsOneTokenAndOnlyToken = case roleTokenConstraints of
      MintRoleTokens _ _ distribution -> do
        (assetId, address) <- Map.toList distribution
        (("roleToken: " <> show assetId <> ": ") <>) <$> do
          let
            cardanoAssetId = toCardanoAssetId assetId
            matches (TxOut outAddress (TxOutValue MultiAssetInBabbageEra value) _ _)
              | selectAsset value cardanoAssetId > 0 = Just (outAddress, value)
              | otherwise = Nothing
            matches (TxOut _ (TxOutAdaOnly era _) _ _) = case era of
          let matchingOuts = mapMaybe matches txOuts
          case matchingOuts of
            [(outAddress, value)] -> do
              fold
                [ check
                    (fmap fst (valueToList value) == [cardanoAssetId])
                    ("Output contains extra tokens: " <> show (fmap fst (valueToList value)))
                , check
                    (selectAsset value cardanoAssetId == 1)
                    ("Output quantity for token expected to equal 1, was: " <> show (selectAsset value cardanoAssetId))
                , check
                    (fromCardanoAddressInEra BabbageEra outAddress == address)
                    ("Output sent to wrong address: " <> show outAddress)
                ]
            [] -> pure "No outputs contain role token"
            _ -> pure "Multiple outputs contain role token"
      _ -> []

    mintsOneToken = case roleTokenConstraints of
      MintRoleTokens _ _ distribution -> case txMintValue of
        TxMintNone
          | Map.null distribution -> []
          | otherwise -> ["No tokens minted"]
        TxMintValue MultiAssetInBabbageEra value _ -> do
          assetId <- Map.keys distribution
          (("roleToken: " <> show assetId <> ": ") <>) <$> do
            let cardanoAssetId = toCardanoAssetId assetId
            let quantityMinted = selectAsset value cardanoAssetId
            check (quantityMinted == 1) ("Expected to mint 1 token, found " <> show quantityMinted)
      _ -> []

mustSpendRoleTokenViolations
  :: MarloweVersion v -> Chain.UTxOs -> TxConstraints v -> TxBodyContent BuildTx BabbageEra -> [String]
mustSpendRoleTokenViolations MarloweV1 utxos TxConstraints{..} TxBodyContent{..} = fold
  [ passThroughUtxo
  ]
  where
    passThroughUtxo = case roleTokenConstraints of
      SpendRoleTokens roleTokens -> do
        roleToken <- Set.toList roleTokens
        (("roleToken: " <> show roleToken <> ": ") <>) <$> do
          let
            isMatch (_, Chain.TransactionOutput{assets = Chain.Assets{tokens = Chain.Tokens tokens}}) =
              Map.member roleToken tokens
            mUtxo = find (isMatch . Chain.toUTxOTuple) $ Chain.toUTxOsList utxos
          case mUtxo of
            Nothing -> ["UTxO not found that contains role token."]
            Just utxo ->
              let
                (txOutRef, transactionOutput) = Chain.toUTxOTuple utxo
              in
                fold
                  [ check
                      (any ((== txOutRef) . fromCardanoTxIn . fst) txIns)
                      ("Expected to consume UTxO " <> show txOutRef)
                  , check
                      (any ((== transactionOutput) . fromCardanoTxOut BabbageEra) txOuts)
                      ("Matching output not found for input " <> show transactionOutput)
                  ]
      _ -> []

mustPayToAddressViolations
  :: MarloweVersion v -> TxConstraints v -> TxBodyContent BuildTx BabbageEra -> [String]
mustPayToAddressViolations MarloweV1 TxConstraints{..} TxBodyContent{..} = do
  (address, assets) <- Map.toList payToAddresses
  (("address: " <> show address <> ": ") <>) <$> do
    let
      totalToAddress = foldMap extractValue
        $ filter ((== address) . extractAddress) txOuts
    check
      (totalToAddress == assets)
      ("Address paid the wrong amount. Expected " <> show assets <> " got " <> show totalToAddress)

mustSendMarloweOutputViolations
  :: MarloweVersion v
  -> MarloweContext v
  -> TxConstraints v
  -> TxBodyContent BuildTx BabbageEra
  -> [String]
mustSendMarloweOutputViolations MarloweV1 MarloweContext{..} TxConstraints{..} TxBodyContent{..} =
  case marloweOutputConstraints of
    MarloweOutputConstraintsNone -> []
    MarloweOutput assets datum ->
      case find ((== marloweAddress) . extractAddress) txOuts of
        Nothing -> ["No output found to Marlowe address"]
        Just txOut -> fold
          [ check (extractValue txOut == assets) "Wrong assets sent to Marlowe Address."
          , check (extractDatum txOut == Just (toChainDatum MarloweV1 datum))
              $ (if isJust $ extractDatum txOut then "Wrong" else "No") <> " datum sent to Marlowe Address."
          ]

mustPayToRoleViolations
  :: MarloweVersion v
  -> MarloweContext v
  -> TxConstraints v
  -> TxBodyContent BuildTx BabbageEra
  -> [String]
mustPayToRoleViolations MarloweV1 MarloweContext{..} TxConstraints{..} TxBodyContent{..} = do
  (roleToken, assets) <- Map.toList payToRoles
  (("roleToken" <> show roleToken <> ": ") <>) <$> do
    let
      isMatch txOut = extractAddress txOut == payoutAddress
                   && extractDatum txOut == Just (toChainPayoutDatum MarloweV1 roleToken)
      matchingOutput = find isMatch txOuts
    case matchingOutput of
      Nothing -> ["No matching output found to payout validator"]
      Just txOut ->
        let
          totalToRole = extractValue txOut
        in
          check
            (totalToRole == assets)
            ("Role paid the wrong amount. Expected " <> show assets <> " got " <> show totalToRole)

mustConsumeMarloweOutputViolations
  :: MarloweVersion v
  -> MarloweContext v
  -> TxConstraints v
  -> TxBodyContent BuildTx BabbageEra
  -> [String]
mustConsumeMarloweOutputViolations MarloweV1 MarloweContext{..} TxConstraints{..} TxBodyContent{..} =
  case marloweInputConstraints of
    MarloweInputConstraintsNone -> []
    MarloweInput invalidBefore invalidHereafter inputs -> fold
      [ fromLeft [] do
          TransactionScriptOutput{..} <- note ["TEST ERROR: MarloweContext doesn't contain script output"] scriptOutput
          (_, witness) <- note
            ["Marlowe script UTxO not consumed"]
            (find ((== utxo) . fromCardanoTxIn . fst) txIns)
          (witDatum, witRedeemer) <- case witness of
            BuildTxWith (ScriptWitness _ (PlutusScriptWitness _ _ _ (ScriptDatumForTxIn d) r _)) ->
              pure (d, r)
            _ -> Left ["TxInput not build with a plutus script witness"]
          Left $ fold
            [ check
                (witDatum == toCardanoScriptData (toChainDatum MarloweV1 datum))
                "Input datum does not match"
            , check
                ( witRedeemer == toCardanoScriptData
                    ( Chain.toDatum $ inputs <&> \case
                      V1.NormalInput content -> V1.Input content
                      V1.MerkleizedInput content hash _ -> V1.MerkleizedTxInput content hash
                    )
                )
                "Input redeemer does not match"
            ]
      , check
          ( txValidityRange ==
              ( TxValidityLowerBound ValidityLowerBoundInBabbageEra invalidBefore
              , TxValidityUpperBound ValidityUpperBoundInBabbageEra invalidHereafter
              )
          )
          "Tx validity range does not match constraints"
      ]

mustConsumePayoutsViolations
  :: MarloweVersion v
  -> MarloweContext v
  -> TxConstraints v
  -> TxBodyContent BuildTx BabbageEra
  -> [String]
mustConsumePayoutsViolations MarloweV1 MarloweContext{..} TxConstraints{..} TxBodyContent{..} = do
  roleToken <- Set.toList payoutInputConstraints
  (("roleToken" <> show roleToken <> ": ") <>) <$> do
    let
      isMatch (_, witness) = case witness of
        BuildTxWith (ScriptWitness _ (PlutusScriptWitness _ _ _ (ScriptDatumForTxIn d) _ _)) ->
          d == toCardanoScriptData (toChainPayoutDatum MarloweV1 roleToken)
        _ -> False
      matchingInputs = fromCardanoTxIn . fst <$> filter isMatch txIns
      isPayoutUtxo utxo = Map.member utxo payoutOutputs
    fold
      [ check (not $ null matchingInputs) "No matching inputs found"
      , check (all isPayoutUtxo matchingInputs) "Not all matching inputs come from the payout address"
      ]

requiresSignatureViolations
  :: MarloweVersion v
  -> Chain.UTxOs
  -> TxConstraints v
  -> TxBodyContent BuildTx BabbageEra
  -> [String]
requiresSignatureViolations MarloweV1 utxos TxConstraints{..} TxBodyContent{..} = do
  pkh <- Set.toList signatureConstraints
  let
    inInput ref = case Chain.lookupUTxO ref utxos of
      Nothing -> False
      Just Chain.TransactionOutput{..} -> case Chain.paymentCredential address of
        Just (Chain.PaymentKeyCredential pkh') -> pkh' == pkh
        _ -> False
    inExtraKeyWits = case txExtraKeyWits of
      TxExtraKeyWitnessesNone -> False
      TxExtraKeyWitnesses _ hashes -> any ((== pkh) . fromCardanoPaymentKeyHash) hashes
    inInputs = any (inInput . fromCardanoTxIn . fst) txIns
  (("pkh" <> show pkh <> ": ") <>) <$> check
    (inExtraKeyWits || inInputs)
    "Witness missing from either extra key wits or inputs"


requiresMetadataViolations
  :: MarloweVersion v -> TxConstraints v -> TxBodyContent BuildTx BabbageEra -> [String]
requiresMetadataViolations MarloweV1 TxConstraints{..} TxBodyContent{..} = do
  (idx, value) <- Map.toList metadataConstraints
  let
    metadata = case txMetadata of
      TxMetadataNone -> Nothing
      TxMetadataInEra _ (TxMetadata md) -> Map.lookup idx md
  (("idx" <> show idx <> ": ") <>) <$> fold
    [ check
      (metadata == Just (Chain.toCardanoMetadata value))
      ("Expected " <> show value <> " got " <> show metadata)
    ]

data SomeTxConstraints = forall v. SomeTxConstraints (MarloweVersion v) (TxConstraints v)

instance Show SomeTxConstraints where
  show (SomeTxConstraints marloweVersion constraints) = case marloweVersion of
    MarloweV1 -> show constraints

instance Arbitrary SomeTxConstraints where
  arbitrary = oneof
    [ SomeTxConstraints MarloweV1 <$> genV1Constraints
    ]
  shrink (SomeTxConstraints marloweVersion constraints) =
    case marloweVersion of
      MarloweV1 -> SomeTxConstraints MarloweV1 <$> shrinkV1Constraints constraints

shrinkV1Constraints :: TxConstraints 'V1 -> [TxConstraints 'V1]
shrinkV1Constraints constraints@TxConstraints{..} = fold
  [ [ constraints {  marloweInputConstraints = x } | x <- shrinkMarloweInputConstraints marloweInputConstraints ]
  , [ constraints {  payoutInputConstraints = x } | x <- shrinkPayoutInputConstraints payoutInputConstraints ]
  , [ constraints {  roleTokenConstraints = x } | x <- shrinkRoleTokenConstraints roleTokenConstraints ]
  , [ constraints {  payToAddresses = x } | x <- shrinkPayToAddresses payToAddresses ]
  , [ constraints {  payToRoles = x } | x <- shrinkPayToRoles payToRoles ]
  , [ constraints {  marloweOutputConstraints = x } | x <- shrinkMarloweOutputConstraints marloweOutputConstraints ]
  , [ constraints {  signatureConstraints = x } | x <- shrinkSignatureConstraints signatureConstraints ]
  , [ constraints {  metadataConstraints = x } | x <- shrinkMetadataConstraints metadataConstraints ]
  ]

shrinkMarloweInputConstraints :: MarloweInputConstraints 'V1 -> [MarloweInputConstraints 'V1]
shrinkMarloweInputConstraints = \case
  MarloweInputConstraintsNone -> []
  MarloweInput s1 s2 redeemer -> MarloweInputConstraintsNone : (MarloweInput s1 s2 <$> shrinkList shrinkNothing redeemer)

shrinkSet :: (a -> [a]) -> Set.Set a -> [Set.Set a]
shrinkSet shrinkItem = fmap Set.fromDistinctAscList . shrinkList shrinkItem . Set.toAscList

shrinkMap :: (v -> [v]) -> Map k v -> [Map k v]
shrinkMap shrinkItem = fmap Map.fromDistinctAscList . shrinkList (traverse shrinkItem) . Map.toAscList

shrinkPayoutInputConstraints :: Set.Set Chain.AssetId -> [Set.Set Chain.AssetId]
shrinkPayoutInputConstraints = shrinkSet shrinkNothing

shrinkRoleTokenConstraints :: RoleTokenConstraints -> [RoleTokenConstraints]
shrinkRoleTokenConstraints = \case
  RoleTokenConstraintsNone -> []
  MintRoleTokens ref witness distribution -> RoleTokenConstraintsNone :
    (MintRoleTokens ref witness <$> shrinkMap shrinkNothing distribution)
  SpendRoleTokens roleTokens -> RoleTokenConstraintsNone :
    (SpendRoleTokens <$> shrinkSet shrinkNothing roleTokens)

shrinkPayToAddresses :: Map Chain.Address Chain.Assets -> [Map Chain.Address Chain.Assets]
shrinkPayToAddresses = shrinkMap shrinkAssets

shrinkPayToRoles :: Map Chain.AssetId Chain.Assets -> [Map Chain.AssetId Chain.Assets]
shrinkPayToRoles = shrinkMap shrinkAssets

shrinkAssets :: Chain.Assets -> [Chain.Assets]
shrinkAssets Chain.Assets{..} = Chain.Assets ada <$> shrinkTokens tokens

shrinkTokens :: Chain.Tokens -> [Chain.Tokens]
shrinkTokens = fmap Chain.Tokens . shrinkMap shrinkNothing . Chain.unTokens

shrinkMarloweOutputConstraints :: MarloweOutputConstraints 'V1 -> [MarloweOutputConstraints 'V1]
shrinkMarloweOutputConstraints = \case
  MarloweOutputConstraintsNone -> []
  MarloweOutput assets datum -> fold
    [ [ MarloweOutput assets' datum | assets' <- shrinkAssets assets ]
    , [ MarloweOutput assets datum' | datum' <- shrinkDatum datum ]
    ]

shrinkDatum :: MarloweData -> [MarloweData]
shrinkDatum md@MarloweData{..} =
  [ md { marloweContract = c, marloweState = s } | (c, s) <- shrink (marloweContract, marloweState) ]

shrinkMerkleizedContinuationsConstraints :: Set.Set (Contract 'V1) -> [Set.Set (Contract 'V1)]
shrinkMerkleizedContinuationsConstraints = shrinkSet shrinkContract

shrinkSignatureConstraints :: Set.Set Chain.PaymentKeyHash -> [Set.Set Chain.PaymentKeyHash]
shrinkSignatureConstraints = shrinkSet shrinkNothing

shrinkMetadataConstraints :: Map Word64 Chain.Metadata -> [Map Word64 Chain.Metadata]
shrinkMetadataConstraints metadata
  | Map.null metadata = []
  | otherwise = [mempty]

genV1Constraints :: Gen (TxConstraints 'V1)
genV1Constraints = sized \n -> frequency
    [ (n, resize (n `div` 2) $ (<>) <$> genV1Constraints <*> genV1Constraints)
    , (1, pure mempty)
    , (1, mustMintRoleToken <$> genTxOutRef <*> genMintScriptWitness <*> genRoleToken <*> genAddress)
    , (1, mustSpendRoleToken <$> genRoleToken)
    , (1, mustPayToAddress <$> genOutAssets <*> genAddress)
    , (1, mustSendMarloweOutput <$> genOutAssets <*> genDatum)
    , (1, mustPayToRole <$> genOutAssets <*> genRoleToken)
    , (1, uncurry mustConsumeMarloweOutput <$> genValidityInterval <*> genInputs)
    , (1, mustConsumePayouts <$> genRoleToken)
    , (1, requiresSignature <$> genPaymentKeyHash)
    , (1, requiresMetadata <$> arbitrary <*> genMetadata)
    ]

removeTokens :: Map Chain.AssetId Chain.Address -> Chain.Assets -> Chain.Assets
removeTokens distribution (Chain.Assets lovelace (Chain.Tokens tokens)) =
  Chain.Assets lovelace $ Chain.Tokens $ Map.difference tokens distribution

genValidityInterval :: Gen (SlotNo, SlotNo)
genValidityInterval = do
  (s1, s2) <- arbitrary
  pure (SlotNo $ min s1 s2, SlotNo $ max s1 s2)

genInputs :: Gen (Inputs 'V1)
genInputs = do
  ctx <- arbitrary
  state <- semiArbitrary ctx
  contract <- semiArbitrary ctx
  txInputs <$> arbitraryValidInput state contract

genMetadata :: Gen Chain.Metadata
genMetadata = hedgehog $ Chain.fromCardanoMetadata <$> genTxMetadataValue

genPaymentKeyHash :: Gen Chain.PaymentKeyHash
genPaymentKeyHash = hedgehog $ fromCardanoPaymentKeyHash . verificationKeyHash <$> genVerificationKey AsPaymentKey

genPayoutDatum :: Gen (PayoutDatum 'V1)
genPayoutDatum = genRoleToken

genContract :: Gen (Contract 'V1)
genContract = semiArbitrary =<< arbitrary

genDatum :: Gen (Datum 'V1)
genDatum = do
  ctx <- arbitrary
  MarloweData <$> (MarloweParams <$> arbitrary) <*> semiArbitrary ctx <*> semiArbitrary ctx

genOutAssets :: Gen Chain.Assets
genOutAssets = hedgehog $ assetsFromCardanoValue <$> genValueForTxOut

genTxOutRef :: Gen Chain.TxOutRef
genTxOutRef = Chain.TxOutRef <$> genTxId <*> (Chain.TxIx <$> arbitrary)

-- NOTE genTxId from Gen.Cardano.Api.Typed is not suitable because it returns a
-- constant txId (so collisions are a really big problem). A technically
-- correct alternative is `hedgehog $ getTxId <$> genTxBody` but this is
-- extremely expensive and slows down the generators dramatically. The
-- compromise is to choose one txId from a set of real TxIds.
genTxId :: Gen Chain.TxId
genTxId = elements
  [ "cb8b240f959c67a391a197804eccb78eb676b25dfea63971f636fc27d9ef52fd"
  , "41be2285a974c0834374a5f8a232014b7790562569074ce7f471ec811f0cec77"
  , "10df1cdc810677b6fd6e6bb078bcce5dde5db67c661e212811448a9bae6859a7"
  , "b8f051f41072f405707617d288f28f878b3f0210c5adcbe8ccc4de2a4e379871"
  , "adf5bf31d0a3423b78d40ed6bed25f41ecbbc08e204bb078d6ec85fa681649f6"
  , "c73d63923073a79cb5ff8f5b22564f272954f8df25c105c972df79cde0b4cfb4"
  , "66fd00fdc82fc848d497529ed364685904a28ff692ddf8395d8e15b467c46e38"
  , "7d7a5b8af9185f76f778a2c32e85a67ed4f23e3b111c0ce46ea71b48150d160a"
  , "c7e55083f4a62f3b78f5e6df30d5c8e32527bc3870766ea74b362e11f122d0bc"
  , "f693e2978cd781c85db1a5040d94e5516c2dab0332cb09867bfa0c938015aec6"
  , "3cd5f0cee6268c7b2fab8ac3175b5aa7b3442b06e2601c519c273c220aec3009"
  , "382c2e4d48cfad516a784e6d9a75cab90c1b801d1520b1fff71b31cf8b8cf508"
  , "880088f7254c8f4e1921d9fcd7bcbc4b8f227a30a9d18172541cb31fcacc726b"
  , "4bd48364b42bea4677b181bf94c9df448421bf0c54c84bb4e22690873954887e"
  , "dea14fd49212a02d8d971b9b7adaba1702a3aee1a98e951e0508babb3ff8adaf"
  , "7b76723a64315db6be7890ae9c30a42bbeeb38e3fb9f15899dc0908b2c6f98d8"
  , "495fbfe0d7102532573bb3b54547abd8e33aa0382bc193e5b740b52644128eb4"
  , "5f37ccfd54406af5888dadcca61f8ae5f1c4e3aff17b6959c5e14d8fd941d793"
  , "90d5abe22513665ab9e7469b2c7323af4bd23094a9f3d254d0f1d0e41d415e9c"
  , "449fcbf49bb5e49b18be9985e7cd69d3788529aa9d39b5073f47aee410c29638"
  , "d620bba4fd25c77dff5379804e8b1800eb05b15e87aa67f869ea5cc38676f281"
  , "adecb0868665a10102c4799fa6855a2cee14a38de85554b0c180aa42b603dd7f"
  , "08753438e67d97ff4c643ee9976a36ae95bbf1a06a3fca4daeeaecafa96e676b"
  , "679f385e216e0992d66bc29f12a68409e47b62b242f373afb820192613a958ad"
  , "d43e089b4741cc0831a903e2c13ba2411cd827feec2a578e9d77a623ed51bd9b"
  , "c0f5a5507786284e6984840e47688cd0a14507605424a2521c300378355bdd1c"
  , "c1ad08b3d4059f625966eab713f4ba1469a64333484dff10182a37d079f2c8d4"
  , "7cde1a77456fe0bd82c0417f82c9bf82733d3aa6f054d3285fe14edb3f08c2f2"
  , "56158e2e8e835c2ce9ef252871fddf02cf803bd7aba8901b060289f5d2bc2464"
  , "8b1fe71c261e902e984ba41590269a70f76958126c4008e374609c0f5fa4efd7"
  , "1b22a4df7d3da5f8d189a6706bddcbf253f68685edba22289a4d0d8d4ccda900"
  , "45c33c3d6b14cac8b660acd0efd9c48eaa4897154a6637cb3b8fffc581b36852"
  , "5da77b031edc90b70ff2d9d96622a5e82b9a945c9c641ac6c44f329aee6c2c02"
  , "39d26eb363856deb4bf983fbf3fea89b67c0ff5e7a4324c71cc981c88ebddae7"
  , "714648cd6d0bf1de75da66834c3ff8d9ff54f29ce382998fd0cc11ea451c5285"
  , "9baed0d7ab2d348f12d07e63f3e62ec1df61d512a013c043ff03473f2211844b"
  , "802c890c2f71c3b4117bd7f41a76594352efb9715c255bb4433a35de9aced0a3"
  , "2f006396fb9b9670a0ba067384540d048a2917428e03b55e5eb31b9f11d04ef6"
  , "92a9e69b277572e1f36263a7f846ba84ef9ea474ae4a561bcbf70d79478b68b1"
  , "dffa768b647867b401d186b91604d1033f73c0d6a80d13dbdc8e7b66a244551a"
  , "69515e7c8efa3f9141e046c418bbd05f17c20fb91d336d5ffe8fd9bd39810888"
  , "0dfb10c368292c8b77021d814e5bbb49fb4611753974bf95999312e0ced1cd70"
  , "886c54cafe6fcfc17466a22400414ad6838c2ad0e351d432fb237bd1bd0fcf79"
  , "b40c009955005315dad94deec5e4239e185fb449464e6d83e5c83883a9fd2f2b"
  , "440ccd5f03252621ca535516c804b2907431c1a97695be8cccb330712a686063"
  , "a292302b26ed3b914636f54044ad57e4236db8d7dbf831200950d5a32d127c4d"
  , "e5baa714e78a85d45ef007b80a915b4ff5b6050b7ad26b7847b68f3cbe894f4a"
  , "ddd56428244c34ea6cd0cc2d92ff2ff79c1b99879749db1e2bdae4392920c56b"
  , "f5221c06c85d4731f25d54df5d0e5f7ba4a712964672f9f2b25e8626d697787a"
  , "cd0206d8165fad475089d718126704caac41601f3ba7429016ed567fcb4cd297"
  , "135a6423bec823820724c88eaa03f641b6410eabe7f51f3ccfa22ab09f278a9d"
  , "939917e81a201631a969ef1a6c2deebb6e57c2d9c7258bc061c142296c49e449"
  , "5e16ad1c2018095ec550a08ebd8bdf1d6f74fdc1dc759a8dda0b27edee95c311"
  , "cd6f4fb7698565f610a40db78bdc7a0b4897c7eda378223d1f5ce8340c26bff8"
  , "1785897bc2468abf247a2109ca0373341bd37551aa7f609c9ebb6ee86a280d3a"
  , "2073073ae3b5014b6f76c16bfb20382614263666196553fce8e849c39bece3d3"
  , "cf5434e808ac90a6cbe307592c2ea8672cb86b1e8f1b3b5cdf0d22a050530a3c"
  , "3cc0876533af216d86f08df740712ad52bf099a12ae8c8d81df85a356f7286ef"
  , "d88c0a703ea402628a296a159e8410a732e08c57c62220c623fe967971fbb475"
  , "d3e96ebec374f10c072b403e6f07ffe533f0bbd13d9dbd01f24b9ac78ec2279f"
  , "a66e0ca2842dc8cd873af85e68bdfcf452b674fd7b5f603782c7d4864b72ec4f"
  , "0b02d9e23e96e2f8f5947e6ac226add43359c34afc86fe2970482d0b60b7eaf8"
  , "22e8cdd799d7c7de2475bc076517fb03855f229c146d6c477d322490905b80b6"
  , "2a2bd25ce895f9235f8421cc500d342af5b07315a76b0bb3ed9bfab0c15b6e79"
  , "cfbd260b5bacf6bf063c74ec3eae9438b386a58f24ad72b4603fab2e22d7219c"
  , "e1b8c48af9f4dfabf92b8500978e9e3af06c6da9139ee489973ceb42ec875e0b"
  , "96dc5b97f8a7de530533d6b812057a5b878298ff14425abefa0da43ca037acc7"
  , "224da92658a1a35821f9f21324ed7b9aeb09ca023660ee1ae168ac1c15bea458"
  , "9e39fff27a033125c176a3bca10e7a5bab471fc883fa839e5b6569cbc1beb1a6"
  , "4ef07b4a7f5261f8f7facc70834789f37da50f7dcb1b5197d6490c0d114b91a0"
  , "f591e43295c0a15883cda658daeb8815ae9f667f150cd98704a0eac2ab1a4bde"
  , "79898783ed4287d72324dd979d275b807e589e5a40dd72b3fe03e295bf137020"
  , "bf3dd6f71e21a6e65c6b0ab444b45dcc854b10d5f8aa431cc29a63349201cb48"
  , "11837b6d83224a3b51b09f40fd341cf13a1149a29f2525e9c29298a585c8f265"
  , "76acb519350d25a781b0345171e3da6840d65fe28141bece75c811eb1e044394"
  , "a72d43b5a10e3c7b750c97283bae5fb0aa36db7b44b0604e815550ff7a034f4e"
  , "0046519ac36c44ef2739a5b17b6d49475828f11a02037d3dbbbb1d663409a82e"
  , "5e631d10f0019395570d09808857b2c9fa9b3bbdeef7081e6d0a62786b77215e"
  , "7c05d520230661de5bed21dabcbbc211e49d8dba96b604360e600480ae1d9d22"
  , "2b16eb1a58206124ebff59f92474fe7853e5fe2af813717e2c049239ef994f29"
  , "f0504414bf68edfa27adb71fadae6950fb02292018909cb6c700ea55fa1eb5f7"
  , "a146de3c069760deeacb8132b979b914ee10e9c2282b245db1c7f41d1e383d04"
  , "4fa9106c10efbc65a9c8b6b92527c6222c2f970b9258096e8f4ce6d6478bec92"
  , "4e65f9fff17d94c5415d0086d9ec573f2d3b66c42375f978927395658e3e641a"
  , "577d3f0111f5bb67b1a0794bdfb596de9998ce249fde1fc5ba3ebd6d820d237e"
  , "1d6ca23dd7348e2c631200c900ad7f0d49d63cdf9d3291f1791f03c3c699b252"
  , "339554c1f558b1b98b10524d4104453eb54fae22b7dfb118caa850efe069f4f3"
  , "2ecebb248671fbe25a8bfc757da41e720fe1b4425eaa82f97abff6a6d057e706"
  , "5853f8a0c295007e1e89287f578158c0db8f57e5a256b15d56331620789a5ff5"
  , "1cad64602aef420c7386ac16ada8260ce609f3dceacd498de8ae4a186f83f958"
  ]

genMintScriptWitness :: Gen (ScriptWitness WitCtxMint BabbageEra)
genMintScriptWitness = oneof
  [ PlutusScriptWitness
      PlutusScriptV1InBabbage
      PlutusScriptV1
      <$> (hedgehog $ PScript <$> genPlutusScript PlutusScriptV1)
      <*> pure NoScriptDatumForMint
      <*> hedgehog genScriptData
      <*> (ExecutionUnits <$> (fromIntegral @Word32 <$> arbitrary) <*> (fromIntegral @Word32 <$> arbitrary))
  ]

genRoleToken :: Gen Chain.AssetId
genRoleToken = Chain.AssetId
  <$> (hedgehog $ fromCardanoPolicyId . PolicyId <$> genScriptHash)
  <*> genRole

-- NOTE just a random list of names that won't conflict with anything generated
-- by Gen.Cardano.Api.Typed
genRole :: Gen Chain.TokenName
genRole = elements
  [ "buyer"
  , "seller"
  , "renter"
  , "lawyer"
  , "oracle"
  , "mediator"
  , "judge"
  , "doctor"
  , "joker"
  , "renegade"
  , "bidder"
  , "opponent"
  , "adversary"
  , "companion"
  , "medic"
  , "wizard"
  , "cleric"
  , "appraiser"
  , "engineer"
  , "auditor"
  , "lender"
  , "borrower"
  , "owner"
  , "applicant"
  ]

genAddress :: Gen Chain.Address
genAddress = fromCardanoAddressAny <$> oneof
  [ hedgehog $ AddressByron <$> genAddressByron
  , hedgehog $ AddressShelley <$> genAddressShelley
  ]

genMarloweContext :: MarloweVersion v -> TxConstraints v -> Gen (MarloweContext v)
genMarloweContext MarloweV1 constraints = do
  marloweScriptHash <- hedgehog genScriptHash
  payoutScriptHash <- hedgehog genScriptHash
  let
    scriptAddress hash = fromCardanoAddressAny
      $ AddressShelley
      $ makeShelleyAddress Mainnet (PaymentCredentialByScript hash) NoStakeAddress
    marloweAddress = scriptAddress marloweScriptHash
    payoutAddress = scriptAddress payoutScriptHash
  MarloweContext
    <$> genScriptOutput marloweAddress constraints
    <*> genPayoutOutputs payoutAddress constraints
    <*> pure marloweAddress
    <*> pure payoutAddress
    <*> genReferenceScriptUtxo marloweAddress
    <*> genReferenceScriptUtxo payoutAddress
    <*> pure (fromCardanoScriptHash marloweScriptHash)
    <*> pure (fromCardanoScriptHash payoutScriptHash)

genScriptOutput :: Chain.Address -> TxConstraints 'V1 -> Gen (Maybe (TransactionScriptOutput 'V1))
genScriptOutput address TxConstraints{..} = case marloweInputConstraints of
  MarloweInputConstraintsNone -> oneof
    [ pure Nothing
    , Just <$> (TransactionScriptOutput address <$> genOutAssets <*> genTxOutRef <*> genDatum)
    ]
  MarloweInput {} -> Just <$> (TransactionScriptOutput address <$> genOutAssets <*> genTxOutRef <*> genDatum)

shrinkPayout :: Payout 'V1 -> [Payout 'V1]
shrinkPayout Payout{..} = Payout address <$> shrinkAssets assets <*> pure datum

genPayoutOutputs :: Chain.Address -> TxConstraints 'V1 -> Gen (Map Chain.TxOutRef (Payout 'V1))
genPayoutOutputs address TxConstraints{..} = (<>) <$> required <*> extra
  where
    required = Map.fromList <$> traverse (genPayout address) (Set.toList payoutInputConstraints)
    extra = Map.fromList <$> listOf (genPayout address =<< genRoleToken)

genPayout :: Chain.Address -> Chain.AssetId -> Gen (Chain.TxOutRef, Payout 'V1)
genPayout address datum = do
  assets <- genOutAssets
  (,Payout{..}) <$> genTxOutRef

shrinkTransactionOutput :: Chain.TransactionOutput -> [Chain.TransactionOutput]
shrinkTransactionOutput Chain.TransactionOutput{..} = Chain.TransactionOutput address
  <$> shrinkAssets assets
  <*> pure datumHash
  <*> pure datum

genReferenceScriptUtxo :: Chain.Address -> Gen ReferenceScriptUtxo
genReferenceScriptUtxo address = ReferenceScriptUtxo
  <$> genTxOutRef
  <*> genTransactionOutput (pure address)
  <*> hedgehog (genPlutusScript PlutusScriptV2)

genTransactionOutput :: Gen Chain.Address -> Gen Chain.TransactionOutput
genTransactionOutput address = Chain.TransactionOutput
  <$> address
  <*> genOutAssets
  <*> pure Nothing
  <*> pure Nothing

genWalletContext :: MarloweVersion v -> TxConstraints v -> Gen WalletContext
genWalletContext MarloweV1 constraints = WalletContext
  <$> genWalletUtxos constraints
  <*> pure mempty
  <*> genAddress

genWalletUtxos :: TxConstraints 'V1 -> Gen Chain.UTxOs
genWalletUtxos TxConstraints{..} = (<>) <$> required <*> extra
  where
    required = case roleTokenConstraints of
      RoleTokenConstraintsNone -> pure mempty
      MintRoleTokens txOutRef _ _ -> Chain.UTxOs . Map.singleton txOutRef <$> genTransactionOutput genAddress
      SpendRoleTokens roleTokens -> fold <$> for (Set.toList roleTokens) \roleToken -> do
        txOutRef <- genTxOutRef
        txOut <- genTransactionOutput genAddress
        let roleTokenAssets = Chain.Assets 0 $ Chain.Tokens $ Map.singleton roleToken 1
        pure $ Chain.UTxOs $ Map.singleton txOutRef $ txOut { Chain.assets = Chain.assets txOut <> roleTokenAssets }
    extra = fold <$> listOf do
      txOutRef <- genTxOutRef
      txOut <- genTransactionOutput genAddress
      pure $ Chain.UTxOs $ Map.singleton txOutRef txOut

toCardanoAssetId :: Chain.AssetId -> AssetId
toCardanoAssetId (Chain.AssetId policy name) = AssetId
  (fromJust $ toCardanoPolicyId policy)
  (toCardanoAssetName name)

extractValue :: TxOut ctx era -> Chain.Assets
extractValue (TxOut _ txOutValue _ _) = fromCardanoTxOutValue txOutValue

extractAddress :: TxOut ctx BabbageEra -> Chain.Address
extractAddress (TxOut addr _ _ _) = fromCardanoAddressInEra BabbageEra addr

extractDatum :: TxOut CtxTx BabbageEra -> Maybe Chain.Datum
extractDatum (TxOut _ _ txOutDatum _) = snd $ fromCardanoTxOutDatum txOutDatum

byteStringGen :: Gen ByteString
byteStringGen = BS.pack <$> arbitrary

protocolTestnet :: ProtocolParameters
protocolTestnet = ProtocolParameters
  { protocolParamProtocolVersion = (8,0)
  , protocolParamDecentralization = Nothing
  , protocolParamExtraPraosEntropy = Nothing
  , protocolParamMaxBlockHeaderSize = 1100
  , protocolParamMaxBlockBodySize = 90112
  , protocolParamMaxTxSize = 16384
  , protocolParamTxFeeFixed = 155381
  , protocolParamTxFeePerByte = 44
  , protocolParamMinUTxOValue = Nothing
  , protocolParamStakeAddressDeposit = Lovelace 2000000
  , protocolParamStakePoolDeposit = Lovelace 500000000
  , protocolParamMinPoolCost = Lovelace 340000000
  , protocolParamPoolRetireMaxEpoch = EpochNo 18
  , protocolParamStakePoolTargetNum = 500
  , protocolParamPoolPledgeInfluence = 3 % 10
  , protocolParamMonetaryExpansion = 3 % 1000
  , protocolParamTreasuryCut = 1 % 5
  , protocolParamUTxOCostPerWord = Nothing
  , protocolParamCostModels = Map.empty
  , protocolParamPrices = Just (ExecutionUnitPrices {priceExecutionSteps = 721 % 10000000
  , priceExecutionMemory = 577 % 10000})
  , protocolParamMaxTxExUnits = Just (ExecutionUnits {executionSteps = 10000000000
  , executionMemory = 14000000})
  , protocolParamMaxBlockExUnits = Just (ExecutionUnits {executionSteps = 40000000000
  , executionMemory = 62000000})
  , protocolParamMaxValueSize = Just 5000
  , protocolParamCollateralPercent = Just 150
  , protocolParamMaxCollateralInputs = Just 3
  , protocolParamUTxOCostPerByte = Just (Lovelace 4310)
  }
