{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TupleSections #-}

module Language.Marlowe.Runtime.Transaction.ConstraintsSpec
  where

import Cardano.Api
import Cardano.Api.Shelley
  ( PlutusScriptOrReferenceInput(..)
  , ProtocolParameters(..)
  , ReferenceScript(ReferenceScriptNone)
  , SimpleScriptOrReferenceInput(SReferenceScript)
  )
import Control.Applicative (Alternative)
import Control.Arrow ((***))
import Control.Error (note)
import Control.Monad (guard)
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Either (fromLeft, isRight)
import Data.Foldable (fold)
import Data.Functor ((<&>))
import Data.List (find, isPrefixOf)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Map.Strict as SMap (filterWithKey, toList)
import Data.Maybe (fromJust, isJust, mapMaybe)
import Data.Monoid (First(..), getFirst)
import Data.Ratio ((%))
import Data.SOP.Strict (K(..), NP(Nil, (:*)))
import qualified Data.Set as Set
import Data.Traversable (for)
import Data.Word (Word32)
import GHC.Word (Word64)
import Gen.Cardano.Api.Typed
  ( genAddressShelley
  , genPlutusScript
  , genProtocolParameters
  , genScriptData
  , genScriptHash
  , genTxBodyContent
  , genTxIn
  , genValueForTxOut
  )
import Language.Marlowe (MarloweData(..), MarloweParams(..), txInputs)
import qualified Language.Marlowe.Core.V1.Semantics.Types as V1
import Language.Marlowe.Runtime.Cardano.Api
import Language.Marlowe.Runtime.ChainSync.Api (fromCardanoPaymentKeyHash, fromCardanoScriptHash, unTransactionMetadata)
import qualified Language.Marlowe.Runtime.ChainSync.Api as Chain
import qualified Language.Marlowe.Runtime.ChainSync.Gen ()
import Language.Marlowe.Runtime.Core.Api
  ( Datum
  , Inputs
  , MarloweVersion(..)
  , MarloweVersionTag(..)
  , Payout(..)
  , TransactionScriptOutput(..)
  , encodeMarloweTransactionMetadata
  , toChainDatum
  , toChainPayoutDatum
  )
import qualified Language.Marlowe.Runtime.Core.Gen ()
import Language.Marlowe.Runtime.Core.ScriptRegistry (ReferenceScriptUtxo(..))
import Language.Marlowe.Runtime.Transaction.Constraints
import qualified Language.Marlowe.Scripts as V1
import Ouroboros.Consensus.BlockchainTime (RelativeTime(..), mkSlotLength)
import Ouroboros.Consensus.HardFork.History
  (Bound(..), EraEnd(..), EraParams(..), EraSummary(..), SafeZone(..), mkInterpreter)
import Ouroboros.Consensus.HardFork.History.Summary (summaryWithExactly)
import Ouroboros.Consensus.Util.Counting (Exactly(..))
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

      (executesPlutusScript, txBodyContent) <-
        frequency
          [
            -- A representative transaction without any scripts, which should not have collateral.
            (1, pure (False, emptyTxBodyContent))
            -- A representative simple script for payment, which should not have collateral.
          , (1, hedgehog $ do
                txIn <- genTxIn
                script <- SReferenceScript <$> genTxIn <*> pure Nothing
                pure (False, emptyTxBodyContent {txIns = [(txIn,
                  BuildTxWith
                    . ScriptWitness ScriptWitnessForSpending
                    $ SimpleScriptWitness SimpleScriptV1InBabbage SimpleScriptV1 script)]}))
            -- A representative transaction with a simple script for minting, which should not have collateral.
          , (1, hedgehog $ do
                policy <- PolicyId <$> genScriptHash
                script <- SReferenceScript <$> genTxIn <*> pure Nothing
                pure (False, emptyTxBodyContent {txMintValue =
                  TxMintValue MultiAssetInBabbageEra mempty
                    $ BuildTxWith
                    $ Map.singleton policy
                    $ SimpleScriptWitness SimpleScriptV1InBabbage SimpleScriptV1 script}))
            -- A representative transaction with a Plutus script for minting, which should have collateral.
          , (5, hedgehog $ do
                policy <- PolicyId <$> genScriptHash
                script <- PReferenceScript <$> genTxIn <*> pure Nothing
                redeemer <- genScriptData
                pure (True, emptyTxBodyContent {txMintValue =
                  TxMintValue MultiAssetInBabbageEra mempty
                    $ BuildTxWith
                    $ Map.singleton policy
                    $ PlutusScriptWitness PlutusScriptV2InBabbage PlutusScriptV2
                        script NoScriptDatumForMint redeemer (ExecutionUnits 0 0)}))
            -- A representative transaction with a Plutus script for payment, which should have collateral.
          , (20, hedgehog $ do
                txIn <- genTxIn
                script <- PReferenceScript <$> genTxIn <*> pure Nothing
                datum <- ScriptDatumForTxIn <$> genScriptData
                redeemer <- genScriptData
                pure (True, emptyTxBodyContent {txIns = [(txIn,
                  BuildTxWith
                    . ScriptWitness ScriptWitnessForSpending
                    $ PlutusScriptWitness PlutusScriptV2InBabbage PlutusScriptV2
                        script datum redeemer (ExecutionUnits 0 0))]}))
          ]

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
        eligible :: [(Chain.TxOutRef, Chain.TransactionOutput)]
        eligible =
          SMap.toList
            . (
                if Set.null $ collateralUtxos walletContext
                  then id
                  else SMap.filterWithKey $ const . flip Set.member (collateralUtxos walletContext)
              )
            . Chain.unUTxOs
            $ availableUtxos walletContext
        utxos :: [TxOut CtxTx BabbageEra]
        utxos = mapMaybe convertUtxo eligible

        -- Compute the value of all available UTxOs
        universe :: Value
        universe = foldMap txOutToValue utxos

        chAddress = changeAddressFromWallet walletContext

        minUtxo = findMinUtxo' protocolTestnet chAddress universe
               <> findMinUtxo' protocolTestnet chAddress mempty

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

        eligibleUtxos txBodyContent' =
          case txInsCollateral txBodyContent' of
            TxInsCollateralNone          -> Left "No collateral selected"
            TxInsCollateral _ collateral -> if all (`elem` mapMaybe (toCardanoTxIn . fst) eligible) collateral
                                              then Right txBodyContent'
                                              else Left "Collateral contains ineligible UTxO"

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
        selection =
          selectCoins protocolTestnet marloweVersion marloweContext
            walletContext txBodyContent
        selectResult :: Either String ()
        selectResult = either
          (\ce -> case marloweVersion of MarloweV1 -> Left . show $ ce)
          (\txBC -> eligibleUtxos txBC >>= singleUtxo . extractCollat >>= assetsAdaOnly >>= adaCollatIsSufficient)
          selection

        noCollateralUnlessPlutus =
          label "Not a Plutus transaction"
            $ case selection of
                Right txBodyContent' ->
                  counterexample "Non-Plutus transaction should not have collateral"
                    $ txInsCollateral txBodyContent' `shouldBe` TxInsCollateralNone
                Left (CoinSelectionFailed message) ->
                  counterexample "Non-Plutus coin selection should not fail due to lack of collateral"
                    $ message `shouldNotSatisfy` isPrefixOf "No collateral found in "
                Left _ -> counterexample "Coin selection may fail for reasons unrelated to collateral" True

      pure $ if executesPlutusScript
        then case (walletCtxSufficient, selectResult) of
          (True , Right _) -> label "Wallet has funds, selection succeeded" True
          (False, Right _) -> counterexample "Selection should have failed" False
          (True , Left selFailedMsg) ->
            counterexample ("Selection shouldn't have failed\n" <> selFailedMsg) False
          (False, Left selFailedMsg) -> label "Wallet does not have funds, selection failed"
            $ selFailedMsg `shouldSatisfy` isPrefixOf "CoinSelectionFailed"
        else noCollateralUnlessPlutus

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

              minUtxo = selectLovelace $ findMinUtxo' protocolTestnet
                (changeAddressFromWallet walletContext) mempty
              maxFee' = maxFee protocolTestnet

            -- check 1: pure ADA inputs - pure ADA outputs >= min utxo + max fee
            selectLovelace (adaIns <> negateValue adaOuts) >= minUtxo <> maxFee' `shouldBe` True
            -- check 2: non-ADA inputs and non-ADA outputs cancel each other out
            tokIns <> negateValue tokOuts `shouldBe` mempty
        Left selFailedMsg -> counterexample ("selection failed: " <> selFailedMsg) False

  describe "findMinUtxo" do
    prop "pure lovelace" do
      inAddress <- arbitrary
      inDatum <- oneof [pure Nothing, Just . fromCardanoScriptData <$> hedgehog genScriptData]
      inValue <- hedgehog genValueForTxOut
      case findMinUtxo protocolTestnet (inAddress, inDatum, inValue) of
        Right outValue  -> pure $ valueToLovelace outValue `shouldSatisfy` isJust
        Left message -> pure . expectationFailure $ show (message :: ConstraintError 'V1)
    prop "minUTxO matches Cardano API" do
      inAddress <- arbitrary
      inDatum <- oneof [pure Nothing, Just <$> hedgehog genScriptData]
      -- Tiny lovelace values violate ledger rules and occupy too few bytes for a meaningful test.
      inValue <- (lovelaceToValue 100_000 <>) <$> hedgehog genValueForTxOut
      either (pure . expectationFailure) pure
        do
         inAddress' <-
           maybe (Left "Failed to convert address.") (Right . anyAddressInShelleyBasedEra)
             $ Chain.toCardanoAddressAny inAddress
         expected <-
           first show
             $ calculateMinimumUTxO
               ShelleyBasedEraBabbage
               (TxOut inAddress' (TxOutValue MultiAssetInBabbageEra inValue) (maybe TxOutDatumNone (TxOutDatumInTx ScriptDataInBabbageEra) inDatum) ReferenceScriptNone)
               protocolTestnet
         outValue <-
           first (\message -> show (message :: ConstraintError 'V1))
             $ findMinUtxo protocolTestnet (inAddress, fromCardanoScriptData <$> inDatum, inValue)
         pure $ (inDatum, outValue) `shouldBe` (inDatum, expected)

  describe "ensureMinUtxo" do
    prop "non-lovelace value is unchanged" do
      let
        noLovelace = valueFromList . filter ((/= AdaAssetId) . fst) . valueToList
      inAddress <- arbitrary
      inValue <- hedgehog genValueForTxOut
      case ensureMinUtxo protocolTestnet (inAddress, inValue) of
        Right (_, outValue)  -> pure $ noLovelace outValue `shouldBe` noLovelace inValue
        Left message -> pure . expectationFailure $ show (message :: ConstraintError 'V1)
    prop "address is unchanged" do
      inAddress <- arbitrary
      inValue <- hedgehog genValueForTxOut
      case ensureMinUtxo protocolTestnet (inAddress, inValue) of
        Right (outAddress, _)  -> pure $ outAddress `shouldBe` inAddress
        Left message -> pure . expectationFailure $ show (message :: ConstraintError 'V1)
    prop "adjusted lovelace is greater of minUTxO and original lovelace" do
      inAddress <- arbitrary
      -- Tiny lovelace values violate ledger rules and occupy too few bytes for a meaningful test.
      inValue <- (lovelaceToValue 100_000 <>) <$> hedgehog genValueForTxOut
      either (pure . expectationFailure) pure
        do
         inAddress' <-
           maybe (Left "Failed to convert address.") (Right . anyAddressInShelleyBasedEra)
             $ Chain.toCardanoAddressAny inAddress
         expected <-
           first show
             $ calculateMinimumUTxO
               ShelleyBasedEraBabbage
               (TxOut inAddress' (TxOutValue MultiAssetInBabbageEra inValue) TxOutDatumNone ReferenceScriptNone)
               protocolTestnet
         (_, outValue) <-
           first (\message -> show (message :: ConstraintError 'V1))
             $ ensureMinUtxo protocolTestnet (inAddress, inValue)
         pure $ selectLovelace outValue `shouldBe` maximum [selectLovelace inValue, selectLovelace expected]

  describe "balanceTx" do
    prop "tx should balance for non-Plutus transactions where the wallet has sufficient funds" \(SomeTxConstraints marloweVersion constraints) -> do
      marloweContext <- genSimpleMarloweContext marloweVersion constraints

      -- We MUST dictate the distribution of wallet context assets, default
      -- generation only tests with empty wallets!
      maxLovelace <- choose (0, 40_000_000)
      walletContext <- genWalletWithAsset marloweVersion constraints maxLovelace
      start <- SystemStart <$> arbitrary

      let
        -- The following 4 definitions are for constructing a pure EraHistory,
        -- which would normally come from the chain at runtime

        eraHistory :: EraHistory CardanoMode
        eraHistory = EraHistory CardanoMode
          $ mkInterpreter
          $ summaryWithExactly
          $ Exactly
          $  K (oneMillisecondEraSummary 0) -- Byron lasted 1 ms
          :* K (oneMillisecondEraSummary 1) -- Shelley lasted 1 ms
          :* K (oneMillisecondEraSummary 2) -- Allegra lasted 1 ms
          :* K (oneMillisecondEraSummary 3) -- Mary lasted 1 ms
          :* K (oneMillisecondEraSummary 4) -- Alonzo lasted 1 ms
          :* K (unboundedEraSummary 5) -- Babbage never ends
          :* Nil

        unboundedEraSummary :: Integer -> EraSummary
        unboundedEraSummary i = EraSummary
          { eraStart = oneMillisecondBound i
          , eraEnd = EraUnbounded
          , eraParams = EraParams
            { eraEpochSize = 1
            , eraSlotLength = mkSlotLength 0.001
            , eraSafeZone = UnsafeIndefiniteSafeZone
            }
          }

        oneMillisecondEraSummary :: Integer -> EraSummary
        oneMillisecondEraSummary i = EraSummary
          { eraStart = oneMillisecondBound i
          , eraEnd = EraEnd $ oneMillisecondBound $ i + 1
          , eraParams = EraParams
            { eraEpochSize = 1
            , eraSlotLength = mkSlotLength 0.001
            , eraSafeZone = UnsafeIndefiniteSafeZone
            }
          }

        oneMillisecondBound :: Integer -> Bound
        oneMillisecondBound i = Bound
          { boundTime = RelativeTime $ fromInteger i / 1000
          , boundSlot = fromInteger i
          , boundEpoch = fromInteger i
          }

        -- We need to make a TxBodyContent that would have come from executing
        -- selectCoins, containing the tx information in the WalletContext we
        -- will also be passing to balanceTx. To do so, we'll use the
        -- walletContext.

        addBuilder :: TxIn -> (TxIn, BuildTxWith BuildTx (Witness WitCtxTxIn BabbageEra))
        addBuilder = (, BuildTxWith (KeyWitness KeyWitnessForSpending))

        txBodyContent = emptyTxBodyContent
          { txIns = map addBuilder . mapMaybe (toCardanoTxIn . fst)
              . Map.toList . Chain.unUTxOs . availableUtxos $ walletContext
          , txInsCollateral = TxInsCollateral CollateralInBabbageEra  -- [TxIn]
              $ mapMaybe toCardanoTxIn . Set.toList . collateralUtxos $ walletContext
          }

      {-  Explanation of the pass/fail criteria below. From a discussion
          between Dino Morelli and Brian Bush 2023-Jan

          In some sense, a successful makeTransactionBodyAutoBalance is the
          ultimate test because that means the tx should succeed when submitted to a
          node.

          Of the errors in TxBodyErrorAutoBalance...

          TxBodyErrorAdaBalanceNegative indicates that balanceTx failed.
          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

          All of the other errors indicate that something upstream from
          balanceTx failed. For instance, errors like TxBodyErrorAssetBalanceWrong,
          TxBodyErrorAdaBalanceNegative,  TxBodyErrorAdaBalanceTooSmall,
          TxBodyErrorMinUTxONotMet, or TxBodyErrorNonAdaAssetsUnbalanced mean that
          selectCoins failed.

          Errors like TxBodyScriptExecutionError or TxBodyErrorValidityInterval
          mean that transaction constraints were wrong or incorrectly solved.

          Only TxBodyScriptExecutionError indicates a Plutus validation failure.
      -}

      pure $ case balanceTx BabbageEraInCardanoMode start eraHistory
              protocolTestnet marloweVersion marloweContext walletContext txBodyContent of
        Right _ -> label "balancing succeeded" True
        Left (BalancingError emsg) -> if "TxBodyErrorAdaBalanceNegative" `isPrefixOf` emsg
          then counterexample ("balancing shouldn't have failed\n" <> emsg) False
          else label "non-balanceable test cases" True
        Left _ -> label "non-balanceable test cases" True

-- Generate a wallet that always has a pure ADA value of 7 and a value
-- with a minimum ADA plus zero or more "nuisance" tokens
genWalletWithNuisance :: MarloweVersion v -> TxConstraints v -> Word64 -> Gen WalletContext
genWalletWithNuisance marloweVersion' constraints' minLovelace = do
  wc <- genWalletContext marloweVersion' constraints'
  (adaTxOutRef, nuisTxOutRef) <- suchThat ((,) <$> arbitrary <*> arbitrary) (uncurry (/=))
  someAddress <- arbitrary
  let lovelaceToAdd = Chain.Assets (Chain.Lovelace minLovelace) (Chain.Tokens Map.empty)
  nuisAssets <- (lovelaceToAdd <>) <$> arbitrary
  collateral <- Set.fromList <$> sublistOf [adaTxOutRef]
  let
    adaAssets = Chain.Assets (Chain.Lovelace 7_000_000) (Chain.Tokens Map.empty)
    adaTxOut = Chain.TransactionOutput someAddress adaAssets Nothing Nothing
    nuisTxOut = Chain.TransactionOutput someAddress nuisAssets Nothing Nothing
    utxos = Chain.UTxOs $ Map.fromList [(adaTxOutRef, adaTxOut), (nuisTxOutRef, nuisTxOut)]
  pure $ wc { availableUtxos = utxos, collateralUtxos = collateral }

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
changeAddressFromWallet = anyAddressInShelleyBasedEra . fromJust . Chain.toCardanoAddressAny . changeAddress

-- FIXME: It's risky to copy-and-paste code being tested into the test suite so that it can be used for other tests.
findMinUtxo' :: ProtocolParameters -> AddressInEra BabbageEra -> Value -> Value
findMinUtxo' protocol chAddress origValue = do
  let
    atLeastHalfAnAda :: Value
    atLeastHalfAnAda = lovelaceToValue (maximum [500_000, selectLovelace origValue])
    revisedValue = origValue <> negateValue (lovelaceToValue $ selectLovelace origValue) <> atLeastHalfAnAda
    dummyTxOut = TxOut
      chAddress (TxOutValue MultiAssetInBabbageEra revisedValue)
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

-- Convenience function to build a chain Assets with the specified amount of only ADA
mkAdaOnlyAssets :: Integer -> Chain.Assets
mkAdaOnlyAssets lovelace = Chain.Assets
  (fromCardanoLovelace $ Lovelace lovelace)
  (Chain.Tokens Map.empty)

-- Generate a random amount and add the specified amount of Lovelace to it
genAtLeastThisMuchAda :: Integer -> Gen Chain.Assets
genAtLeastThisMuchAda minLovelace = do
  additionalLovelaceValue <- suchThat arbitrary (>= 0)
  pure . mkAdaOnlyAssets $ minLovelace + additionalLovelaceValue

-- The simplest wallet context:
--   availableUtxos = A single ADA-only Utxo
--   collateralUtxos = A set containing the one Utxo from above
--   changeAddress = any valid address
genWalletWithAsset :: MarloweVersion v -> TxConstraints v -> Integer -> Gen WalletContext
genWalletWithAsset marloweVersion constraints minLovelace = do
  wc <- genWalletContext marloweVersion constraints
  txOutRef <- arbitrary
  stubAddress <- arbitrary
  assets <- genAtLeastThisMuchAda minLovelace
  collateral <- Set.fromList <$> sublistOf [txOutRef]
  let
    txOut = Chain.TransactionOutput stubAddress assets Nothing Nothing
    utxos = Chain.UTxOs $ Map.singleton txOutRef txOut
  pure $ wc { availableUtxos = utxos, collateralUtxos = collateral }

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
  (idx, value) <- Map.toList $ unTransactionMetadata $ encodeMarloweTransactionMetadata metadataConstraints
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
  , [ constraints {  payoutInputConstraints = x } | x <- shrink payoutInputConstraints ]
  , [ constraints {  roleTokenConstraints = x } | x <- shrinkRoleTokenConstraints roleTokenConstraints ]
  , [ constraints {  payToAddresses = x } | x <- shrink payToAddresses ]
  , [ constraints {  payToRoles = x } | x <- shrink payToRoles ]
  , [ constraints {  marloweOutputConstraints = x } | x <- shrinkMarloweOutputConstraints marloweOutputConstraints ]
  , [ constraints {  signatureConstraints = x } | x <- shrink signatureConstraints ]
  , [ constraints {  metadataConstraints = x } | x <- shrink metadataConstraints ]
  ]

shrinkMarloweInputConstraints :: MarloweInputConstraints 'V1 -> [MarloweInputConstraints 'V1]
shrinkMarloweInputConstraints = \case
  MarloweInputConstraintsNone -> []
  MarloweInput s1 s2 redeemer -> MarloweInputConstraintsNone : (MarloweInput s1 s2 <$> shrinkList shrinkNothing redeemer)

shrinkSet :: (a -> [a]) -> Set.Set a -> [Set.Set a]
shrinkSet shrinkItem = fmap Set.fromDistinctAscList . shrinkList shrinkItem . Set.toAscList

shrinkMap :: (v -> [v]) -> Map k v -> [Map k v]
shrinkMap shrinkItem = fmap Map.fromDistinctAscList . shrinkList (traverse shrinkItem) . Map.toAscList

shrinkRoleTokenConstraints :: RoleTokenConstraints -> [RoleTokenConstraints]
shrinkRoleTokenConstraints = \case
  RoleTokenConstraintsNone -> []
  MintRoleTokens ref witness distribution -> RoleTokenConstraintsNone :
    (MintRoleTokens ref witness <$> shrinkMap shrinkNothing distribution)
  SpendRoleTokens roleTokens -> RoleTokenConstraintsNone :
    (SpendRoleTokens <$> shrinkSet shrinkNothing roleTokens)

shrinkMarloweOutputConstraints :: MarloweOutputConstraints 'V1 -> [MarloweOutputConstraints 'V1]
shrinkMarloweOutputConstraints = \case
  MarloweOutputConstraintsNone -> []
  MarloweOutput assets datum -> fold
    [ [ MarloweOutput assets' datum | assets' <- shrink assets ]
    , [ MarloweOutput assets datum' | datum' <- shrink datum ]
    ]

genV1Constraints :: Gen (TxConstraints 'V1)
genV1Constraints = sized \n -> frequency
    [ (n, resize (n `div` 2) $ (<>) <$> genV1Constraints <*> genV1Constraints)
    , (1, pure mempty)
    , (1, mustMintRoleToken <$> arbitrary <*> genMintScriptWitness <*> genRoleToken <*> arbitrary)
    , (1, mustSpendRoleToken <$> genRoleToken)
    , (1, mustPayToAddress <$> arbitrary <*> arbitrary)
    , (1, mustSendMarloweOutput <$> arbitrary <*> genDatum)
    , (1, mustPayToRole <$> arbitrary <*> genRoleToken)
    , (1, uncurry mustConsumeMarloweOutput <$> genValidityInterval <*> genInputs)
    , (1, mustConsumePayouts <$> genRoleToken)
    , (1, requiresSignature <$> arbitrary)
    , (1, requiresMetadata <$> arbitrary)
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

genDatum :: Gen (Datum 'V1)
genDatum = do
  ctx <- arbitrary
  MarloweData <$> (MarloweParams <$> arbitrary) <*> semiArbitrary ctx <*> semiArbitrary ctx

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
    , Just <$> (TransactionScriptOutput address <$> arbitrary <*> arbitrary <*> genDatum)
    ]
  MarloweInput {} -> Just <$> (TransactionScriptOutput address <$> arbitrary <*> arbitrary <*> genDatum)

genPayoutOutputs :: Chain.Address -> TxConstraints 'V1 -> Gen (Map Chain.TxOutRef (Payout 'V1))
genPayoutOutputs address TxConstraints{..} = (<>) <$> required <*> extra
  where
    required = Map.fromList <$> traverse (genPayout address) (Set.toList payoutInputConstraints)
    extra = Map.fromList <$> listOf (genPayout address =<< genRoleToken)

genPayout :: Chain.Address -> Chain.AssetId -> Gen (Chain.TxOutRef, Payout 'V1)
genPayout address datum = do
  assets <- arbitrary
  (,Payout{..}) <$> arbitrary

genReferenceScriptUtxo :: Chain.Address -> Gen ReferenceScriptUtxo
genReferenceScriptUtxo address = ReferenceScriptUtxo
  <$> arbitrary
  <*> genTransactionOutput (pure address)
  <*> hedgehog (genPlutusScript PlutusScriptV2)

genTransactionOutput :: Gen Chain.Address -> Gen Chain.TransactionOutput
genTransactionOutput address = Chain.TransactionOutput
  <$> address
  <*> arbitrary
  <*> pure Nothing
  <*> pure Nothing

genWalletContext :: MarloweVersion v -> TxConstraints v -> Gen WalletContext
genWalletContext MarloweV1 constraints = WalletContext
  <$> genWalletUtxos constraints
  <*> pure mempty
  <*> arbitrary

genWalletUtxos :: TxConstraints 'V1 -> Gen Chain.UTxOs
genWalletUtxos TxConstraints{..} = (<>) <$> required <*> extra
  where
    required = case roleTokenConstraints of
      RoleTokenConstraintsNone -> pure mempty
      MintRoleTokens txOutRef _ _ -> Chain.UTxOs . Map.singleton txOutRef <$> genTransactionOutput arbitrary
      SpendRoleTokens roleTokens -> fold <$> for (Set.toList roleTokens) \roleToken -> do
        txOutRef <- arbitrary
        txOut <- genTransactionOutput arbitrary
        let roleTokenAssets = Chain.Assets 0 $ Chain.Tokens $ Map.singleton roleToken 1
        pure $ Chain.UTxOs $ Map.singleton txOutRef $ txOut { Chain.assets = Chain.assets txOut <> roleTokenAssets }
    extra = fold <$> listOf do
      txOutRef <- arbitrary
      txOut <- genTransactionOutput arbitrary
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
