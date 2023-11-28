{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Language.Marlowe.Runtime.Transaction.BuildConstraintsSpec (
  spec,
) where

import Cardano.Api (
  AssetId (..),
  AssetName (..),
  BabbageEra,
  ConsensusMode (..),
  EraHistory (EraHistory),
  MultiAssetSupportedInEra (..),
  SlotNo (SlotNo),
  TxBody (..),
  TxBodyContent (..),
  TxOut (..),
  TxOutValue (..),
  selectAsset,
  toLedgerEpochInfo,
 )
import qualified Cardano.Api as C
import Cardano.Api.Shelley (ReferenceTxInsScriptsInlineDatumsSupportedInEra (..))
import Control.Monad.Trans.Except (runExcept, runExceptT)
import Data.Function (on)
import Data.Functor ((<&>))
import Data.Functor.Identity (Identity (..))
import Data.List (isPrefixOf)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Map.NonEmpty as NEMap
import Data.Maybe (fromJust, maybeToList)
import Data.Monoid (Sum (..))
import Data.SOP.Counting (Exactly (..))
import Data.SOP.Strict (K (..), NP (..))
import qualified Data.Set as Set
import Data.Time (UTCTime, nominalDiffTimeToSeconds, secondsToNominalDiffTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import Data.Traversable (for)
import GHC.Generics (Generic)
import qualified Language.Marlowe as V1
import qualified Language.Marlowe.Core.V1.Semantics as Semantics
import qualified Language.Marlowe.Core.V1.Semantics.Types as Semantics
import qualified Language.Marlowe.Core.V1.Semantics.Types.Address as Semantics
import qualified Language.Marlowe.Core.V1.Semantics.Types.Address as V1
import Language.Marlowe.Runtime.Cardano.Api (fromCardanoPolicyId, toCardanoPlutusScript, toCardanoPolicyId)
import Language.Marlowe.Runtime.ChainSync.Api (Lovelace, PlutusScript (..), fromBech32, toUTxOsList)
import qualified Language.Marlowe.Runtime.ChainSync.Api as Chain
import Language.Marlowe.Runtime.Core.Api (
  Contract,
  Datum,
  MarloweTransactionMetadata,
  MarloweVersion (..),
  MarloweVersionTag (..),
  TransactionScriptOutput (..),
  emptyMarloweTransactionMetadata,
 )
import qualified Language.Marlowe.Runtime.Core.Api as Core.Api
import Language.Marlowe.Runtime.Core.ScriptRegistry (
  HelperScript (..),
  ReferenceScriptUtxo (..),
  node812V1MarloweScript,
  node812V1PayoutScript,
 )
import Language.Marlowe.Runtime.Plutus.V2.Api (fromPlutusValue, toAssetId)
import Language.Marlowe.Runtime.Transaction.Api (
  ApplyInputsConstraintsBuildupError (..),
  ApplyInputsError (..),
  CreateError,
  Destination (..),
  Mint (..),
  MintRole (..),
  RoleTokensConfig (..),
 )
import qualified Language.Marlowe.Runtime.Transaction.Api as Transaction.Api
import Language.Marlowe.Runtime.Transaction.BuildConstraints (
  buildApplyInputsConstraints,
  buildCreateConstraints,
  safeLovelace,
 )
import qualified Language.Marlowe.Runtime.Transaction.BuildConstraints as BuildConstraints
import Language.Marlowe.Runtime.Transaction.Constraints (
  Distribution (..),
  HelpersContext (..),
  MarloweContext (..),
  MarloweInputConstraints (..),
  MarloweOutputConstraints (..),
  PayoutContext (..),
  RoleTokenConstraints (..),
  TxConstraints (..),
  WalletContext (..),
  solveConstraints,
 )
import qualified Language.Marlowe.Runtime.Transaction.Constraints as TxConstraints
import Language.Marlowe.Runtime.Transaction.ConstraintsSpec (protocolTestnet)
import qualified Language.Marlowe.Runtime.Transaction.Gen ()
import Ouroboros.Consensus.BlockchainTime (RelativeTime (..), SystemStart (..), mkSlotLength)
import Ouroboros.Consensus.HardFork.History (
  Bound (..),
  EraEnd (..),
  EraParams (..),
  EraSummary (..),
  SafeZone (..),
  mkInterpreter,
  summaryWithExactly,
 )
import PlutusLedgerApi.V1 (Address (Address), Credential (PubKeyCredential), PubKeyHash (PubKeyHash), fromBuiltin)
import PlutusLedgerApi.V1.Time (POSIXTime (POSIXTime))
import qualified PlutusTx.AssocMap as AM
import Spec.Marlowe.Semantics.Arbitrary ()
import Test.Hspec (Spec, it, shouldBe)
import qualified Test.Hspec as Hspec
import qualified Test.Hspec.QuickCheck as Hspec.QuickCheck
import Test.QuickCheck (
  Arbitrary (..),
  Property,
  chooseInteger,
  counterexample,
  elements,
  forAllShrink,
  genericShrink,
  listOf,
  oneof,
  suchThat,
  (===),
  (==>),
 )
import qualified Test.QuickCheck as QuickCheck
import Test.QuickCheck.Instances ()

spec :: Spec
spec = do
  createSpec
  withdrawSpec
  buildApplyInputsConstraintsSpec

createSpec :: Spec
createSpec = Hspec.describe "buildCreateConstraints" do
  emptyStateProp "writes state with empty choices to marlowe output" $ const Semantics.choices
  emptyStateProp "writes state with empty bound values to marlowe output" $ const Semantics.boundValues
  emptyStateProp "writes state with min time 0 to marlowe output" $ const Semantics.minTime
  Hspec.QuickCheck.prop "writes the contract to the marlowe output" \(SomeCreateArgs args) ->
    let result = extractMarloweDatum <$> runBuildCreateConstraints args
     in case version args of
          MarloweV1 -> (fmap Semantics.marloweContract <$> result) === (Right $ Just $ contract args)
          :: Property
  Hspec.QuickCheck.prop "sends the minAda deposit to the marlowe output" \(SomeCreateArgs args) ->
    let result = fmap Chain.ada . extractMarloweAssets <$> runBuildCreateConstraints args
     in case version args of
          MarloweV1 -> result === (Right $ Just $ minAda args)
          :: Property
  Hspec.QuickCheck.prop "sends minted role tokens to the right destinations" \(SomeCreateArgs args) ->
    let result = extractSentRoleTokens <$> runBuildCreateConstraints args
        expected = getRolesForAddresses $ roleTokensConfig args
     in case version args of
          MarloweV1 -> result === Right expected
          :: Property
  Hspec.QuickCheck.prop "total balance == marlowe output assets" \(SomeCreateArgs args) ->
    let result = runBuildCreateConstraints args
        mDatum = extractMarloweDatum <$> result
        mAssets = extractMarloweAssets <$> result
     in case version args of
          MarloweV1 ->
            (fmap (fromPlutusValue . Semantics.totalBalance . Semantics.accounts . Semantics.marloweState) <$> mDatum) === mAssets
          :: Property
  Hspec.QuickCheck.prop "Doesn't send any payments to addresses" \(SomeCreateArgs args) ->
    let result = payToAddresses <$> runBuildCreateConstraints args
     in case version args of
          MarloweV1 -> result === Right mempty
          :: Property
  Hspec.QuickCheck.prop "Doesn't send any payments to roles" \(SomeCreateArgs args) ->
    let result = payToRoles <$> runBuildCreateConstraints args
     in case version args of
          MarloweV1 -> result === Right mempty
          :: Property
  Hspec.QuickCheck.prop "Doesn't consume any roles" \(SomeCreateArgs args) ->
    let result = payoutInputConstraints <$> runBuildCreateConstraints args
     in case version args of
          MarloweV1 -> result === Right mempty
          :: Property
  Hspec.QuickCheck.prop "Doesn't consume a marlowe input" \(SomeCreateArgs args) ->
    let result = marloweInputConstraints <$> runBuildCreateConstraints args
     in case version args of
          MarloweV1 -> result === Right MarloweInputConstraintsNone
          :: Property
  Hspec.QuickCheck.prop "Doesn't require extra signatures" \(SomeCreateArgs args) ->
    let result = signatureConstraints <$> runBuildCreateConstraints args
     in case version args of
          MarloweV1 -> result === Right mempty
          :: Property
  Hspec.QuickCheck.prop "Writes the correct metadata" \(SomeCreateArgs args) ->
    let result = metadataConstraints <$> runBuildCreateConstraints args
     in case version args of
          MarloweV1 -> result === Right (metadata args)
          :: Property
  Hspec.QuickCheck.prop "Adds thread tokens to the initial state" \(SomeCreateArgs args) ->
    hasOpenRoles args ==>
      let threadTokenAssetId = Chain.AssetId (getPolicyId args) $ threadName args
          constraints = runBuildCreateConstraints args
          assets = extractMarloweAssets <$> constraints
          result = (Map.lookup threadTokenAssetId . Chain.unTokens . Chain.tokens =<<) <$> assets
       in case version args of
            MarloweV1 ->
              counterexample (show threadTokenAssetId)
                . counterexample (show constraints)
                . counterexample (show assets)
                $ result === Right (Just 1)
            :: Property
  where
    emptyStateProp :: (Eq a, Show a) => String -> (CreateArgs 'V1 -> Semantics.State -> a) -> Spec
    emptyStateProp name f = Hspec.QuickCheck.prop name \(SomeCreateArgs args) ->
      let result = extractMarloweDatum <$> runBuildCreateConstraints args
       in case version args of
            MarloweV1 ->
              on
                (===)
                ((fmap . fmap) (f args))
                (fmap Semantics.marloweState <$> result)
                (Right (Just $ Semantics.emptyState 0))
            :: Property

hasOpenRoles :: CreateArgs v -> Bool
hasOpenRoles CreateArgs{..} = case roleTokensConfig of
  RoleTokensNone -> False
  RoleTokensMint (Mint mint) -> any (NEMap.member (ToScript OpenRoleScript) . roleTokenRecipients) mint
  RoleTokensUsePolicy _ dist -> any (Map.member (ToScript OpenRoleScript)) dist

testMintingValidator :: PlutusScript
testMintingValidator = PlutusScript mempty

getPolicyId :: CreateArgs v -> Chain.PolicyId
getPolicyId CreateArgs{..} = case roleTokensConfig of
  RoleTokensNone -> ""
  RoleTokensUsePolicy p _ -> p
  RoleTokensMint _ -> testRoleTokensPolicyId

testRoleTokensPolicyId :: Chain.PolicyId
testRoleTokensPolicyId =
  fromCardanoPolicyId
    . C.PolicyId
    . C.hashScript
    . C.PlutusScript C.PlutusScriptV2
    . fromJust
    $ toCardanoPlutusScript testMintingValidator

getRolesForAddresses :: RoleTokensConfig -> Map (Chain.TokenName, Destination) Chain.Quantity
getRolesForAddresses =
  Map.filter (> 0) . \case
    RoleTokensNone -> mempty
    RoleTokensUsePolicy _ dist -> flattenMap dist
    RoleTokensMint (Mint mint) -> flattenMap $ NEMap.toMap $ NEMap.toMap . roleTokenRecipients <$> mint

flattenMap :: Map a (Map b c) -> Map (a, b) c
flattenMap abc = Map.fromDistinctAscList do
  (a, bc) <- Map.toAscList abc
  (b, c) <- Map.toAscList bc
  pure ((a, b), c)

extractSentRoleTokens
  :: TxConstraints BabbageEra v
  -> Map (Chain.TokenName, Destination) Chain.Quantity
extractSentRoleTokens TxConstraints{..} = case roleTokenConstraints of
  RoleTokenConstraintsNone -> mempty
  SpendRoleTokens{} -> mempty
  MintRoleTokens _ _ mintPlan -> go mintPlan
  DistributeRoleTokens mintPlan -> go mintPlan
  where
    go = \case
      SendToScripts _ dist -> Map.fromList do
        (Chain.AssetId _ token, dist') <- Map.toList dist
        (dest, q) <- Map.toList dist'
        pure ((token, dest), q)
      SendToAddresses dist -> Map.fromList do
        (Chain.AssetId _ token, dist') <- Map.toList dist
        (addr, q) <- Map.toList dist'
        pure ((token, ToAddress addr), q)

extractMarloweDatum :: TxConstraints BabbageEra v -> Maybe (Datum v)
extractMarloweDatum TxConstraints{..} = case marloweOutputConstraints of
  MarloweOutput _ datum -> Just datum
  _ -> Nothing

extractMarloweAssets :: TxConstraints BabbageEra v -> Maybe Chain.Assets
extractMarloweAssets TxConstraints{..} = case marloweOutputConstraints of
  MarloweOutput assets _ -> Just assets
  _ -> Nothing

runBuildCreateConstraints :: CreateArgs v -> Either CreateError (TxConstraints BabbageEra v)
runBuildCreateConstraints CreateArgs{..} =
  snd
    <$> runIdentity
      ( buildCreateConstraints
          -- Since we don't actually run the script, we can just return empty bytes
          (\_ _ -> pure testMintingValidator)
          ReferenceTxInsScriptsInlineDatumsInBabbageEra
          version
          walletContext
          threadName
          roleTokensConfig
          metadata
          minAda
          (\(Chain.Assets ada tokens) -> Chain.Assets ada tokens)
          contract
      )

data CreateArgs v = CreateArgs
  { version :: MarloweVersion v
  , walletContext :: WalletContext
  , threadName :: Chain.TokenName
  , roleTokensConfig :: RoleTokensConfig
  , metadata :: MarloweTransactionMetadata
  , minAda :: Lovelace
  , contract :: Contract v
  }
  deriving (Generic)

instance Arbitrary (CreateArgs 'V1) where
  arbitrary = do
    CreateArgs MarloweV1
      <$> arbitrary `suchThat` notEmptyWalletContext
      <*> arbitrary
      <*> (noMetadata <$> arbitrary)
      <*> arbitrary
      <*> ((+ safeLovelace) . Chain.Lovelace <$> arbitrary)
      <*> arbitrary
  shrink args@CreateArgs{..} =
    concat
      [ [args{contract = x} | x <- shrink contract]
      , [args{walletContext = x} | x <- shrink walletContext]
      , [args{threadName = x} | x <- shrink threadName]
      , [args{roleTokensConfig = x} | x <- shrink roleTokensConfig]
      , [args{metadata = x} | x <- shrink metadata]
      ]

noMetadata :: RoleTokensConfig -> RoleTokensConfig
noMetadata = \case
  RoleTokensMint (Mint mint) -> RoleTokensMint $ Mint $ noMetadata' <$> mint
  x -> x

noMetadata' :: MintRole -> MintRole
noMetadata' MintRole{..} = MintRole{roleMetadata = Nothing, ..}

instance Arbitrary WalletContext where
  arbitrary = WalletContext <$> arbitrary <*> arbitrary <*> arbitrary
  shrink = genericShrink

notEmptyWalletContext :: WalletContext -> Bool
notEmptyWalletContext WalletContext{..} = not $ null $ toUTxOsList availableUtxos

deriving instance Show (CreateArgs 'V1)

data SomeCreateArgs = forall v. SomeCreateArgs (CreateArgs v)

instance Arbitrary SomeCreateArgs where
  arbitrary = SomeCreateArgs @'V1 <$> arbitrary
  shrink (SomeCreateArgs args) = case version args of
    MarloweV1 -> SomeCreateArgs <$> shrink args

instance Show SomeCreateArgs where
  showsPrec p (SomeCreateArgs a) = case version a of
    MarloweV1 -> showsPrec p a

withdrawSpec :: Spec
withdrawSpec = Hspec.describe "buildWithdrawConstraints" do
  Hspec.QuickCheck.prop "builds the correct constraints" \payouts' payout -> do
    let payouts = Set.insert payout payouts'
    forAllShrink (genPayoutContext payouts) shrinkPayoutContext \(roleTokens, payoutContext) -> do
      let actual :: Either Transaction.Api.WithdrawError (TxConstraints BabbageEra 'Core.Api.V1)
          actual = runIdentity $ runExceptT $ snd <$> BuildConstraints.buildWithdrawConstraints payoutContext Core.Api.MarloweV1 payouts

          expected :: Either Transaction.Api.WithdrawError (TxConstraints BabbageEra 'Core.Api.V1)
          expected =
            Right $
              TxConstraints
                { marloweInputConstraints = TxConstraints.MarloweInputConstraintsNone
                , payoutInputConstraints = payouts
                , roleTokenConstraints = TxConstraints.SpendRoleTokens roleTokens
                , payToAddresses = Map.empty
                , payToRoles = Map.empty
                , marloweOutputConstraints = TxConstraints.MarloweOutputConstraintsNone
                , signatureConstraints = Set.empty
                , metadataConstraints = emptyMarloweTransactionMetadata
                }

      actual `shouldBe` expected

shrinkPayoutContext :: (Set.Set Chain.AssetId, PayoutContext) -> [(Set.Set Chain.AssetId, PayoutContext)]
shrinkPayoutContext (roleTokens, PayoutContext{..}) = (roleTokens,) <$> contextShrinks
  where
    contextShrinks = flip PayoutContext payoutScriptOutputs <$> foldMap shrinkPayoutOutput (Map.keys payoutOutputs)

    shrinkPayoutOutput :: Chain.TxOutRef -> [Map Chain.TxOutRef Chain.TransactionOutput]
    shrinkPayoutOutput payout = do
      Chain.TransactionOutput{..} <- maybeToList $ Map.lookup payout payoutOutputs
      flip (Map.insert payout) payoutOutputs
        <$> [Chain.TransactionOutput{address = address', ..} | address' <- shrink address]
          <> [Chain.TransactionOutput{assets = assets', ..} | assets' <- shrink assets]

genPayoutContext :: Set.Set Chain.TxOutRef -> QuickCheck.Gen (Set.Set Chain.AssetId, TxConstraints.PayoutContext)
genPayoutContext payouts = do
  relations <- for (Set.toAscList payouts) \payout -> do
    roleToken <- arbitrary
    output <- arbitrary
    pure (payout, roleToken, output{Chain.datum = Just $ Core.Api.toChainPayoutDatum MarloweV1 roleToken})
  pure
    ( Set.fromList $ relations <&> \(_, roleToken, _) -> roleToken
    , PayoutContext
        { payoutOutputs = Map.fromDistinctAscList $ relations <&> \(payout, _, txOut) -> (payout, txOut)
        , payoutScriptOutputs = mempty
        }
    )

buildApplyInputsConstraintsSpec :: Spec
buildApplyInputsConstraintsSpec =
  Hspec.describe "buildApplyInputsConstraints" do
    -- TODO: Break up this preface to utility functions when additional properties are tested.
    let -- System start and era history are reusable across tests.
        systemStart = SystemStart $ posixSecondsToUTCTime 0 -- Without loss of generality.
        eraParams =
          EraParams
            { eraEpochSize = 1
            , eraSlotLength = mkSlotLength 1
            , eraSafeZone = UnsafeIndefiniteSafeZone
            }
        oneSecondBound i =
          Bound
            { boundTime = RelativeTime $ fromInteger i
            , boundSlot = fromInteger i
            , boundEpoch = fromInteger i
            }
        oneSecondEraSummary i =
          EraSummary
            { eraStart = oneSecondBound i
            , eraEnd = EraEnd $ oneSecondBound $ i + 1
            , eraParams
            }
        unboundedEraSummary i =
          EraSummary
            { eraStart = oneSecondBound i
            , eraEnd = EraUnbounded
            , eraParams
            }
        eraHistory =
          EraHistory CardanoMode
            . mkInterpreter
            . summaryWithExactly
            $ Exactly
            $ K (oneSecondEraSummary 0) -- Byron lasted 1 second
              :* K (oneSecondEraSummary 1) -- Shelley lasted 1 second
              :* K (oneSecondEraSummary 2) -- Allegra lasted 1 second
              :* K (oneSecondEraSummary 3) -- Mary lasted 1 second
              :* K (oneSecondEraSummary 4) -- Alonzo lasted 1 second
              :* K (oneSecondEraSummary 5) -- Babbage lasted 1 second
              :* K (unboundedEraSummary 6) -- Conway never ends
              :* Nil
        -- Important note: these slot computations cannot be used generally, but are specifically tailored
        -- to the contrived era history and system start used for this test case.
        genTipSlot = chooseInteger (9, 20) -- Make sure the tip is in the Babbage era.
        genMinTime =
          oneof
            [ (1000 *) <$> chooseInteger (6, 20) -- Even seconds, so there is a chance of collision.
            , chooseInteger (6000, 20000) -- Milliseconds, so there is rounding off.
            ]
        genTimeout =
          oneof
            [ (1000 *) <$> chooseInteger (6, 20) -- Even seconds, so there is a chance of collision.
            , chooseInteger (6000, 20000) -- Milliseconds, so there is rounding off.
            ]
        toSlot = (`div` 1000)
        toSlotNo = SlotNo . fromInteger . (`div` 1000)
        -- Time conversions.
        toUTC = posixSecondsToUTCTime . secondsToNominalDiffTime . (/ 1000) . fromInteger :: Integer -> UTCTime
        fromUTC = floor . (1000 *) . nominalDiffTimeToSeconds . utcTimeToPOSIXSeconds :: UTCTime -> Integer
        toSecondFloor = (* 1000) . (`div` 1000)
        distantFuture = 1_000_000_000_000_000_000_000
        -- Chain conversions.
        toChainAddress = (Chain.Address .) . Semantics.serialiseAddress
        toChainAssets (Semantics.Token "" "") amount = Chain.Assets (fromInteger amount) mempty
        toChainAssets (Semantics.Token currency name) amount = Chain.Assets 0 . Chain.Tokens $ Map.singleton (toAssetId currency name) (fromInteger amount)
        toAction (Semantics.IDeposit account party token amount) = Semantics.Deposit account party token $ Semantics.Constant amount
        toAction (Semantics.IChoice choiceId chosenNum) = Semantics.Choice choiceId [Semantics.Bound chosenNum chosenNum]
        toAction Semantics.INotify = Semantics.Notify Semantics.TrueObs
        toContract action contract = Semantics.When [Semantics.Case action contract] distantFuture Semantics.Close
        toChainRole = toAssetId . Semantics.rolesCurrency
        toRole marloweParams (Semantics.IDeposit _ (Semantics.Role name) _ _) = Set.singleton $ toChainRole marloweParams name
        toRole marloweParams (Semantics.IChoice (Semantics.ChoiceId _ (Semantics.Role name)) _) = Set.singleton $ toChainRole marloweParams name
        toRole _ _ = Set.empty
        toAddress (Semantics.IDeposit _ (Semantics.Address _ address') _ _) = toPaymentKeyHash address'
        toAddress (Semantics.IChoice (Semantics.ChoiceId _ (Semantics.Address _ address')) _) = toPaymentKeyHash address'
        toAddress _ = Set.empty
        toPaymentKeyHash (Address (PubKeyCredential (PubKeyHash hash)) _) = Set.singleton . Chain.PaymentKeyHash $ fromBuiltin hash
        toPaymentKeyHash _ = Set.empty
        -- Contracts.
        assertCloseContract = Semantics.Assert Semantics.TrueObs Semantics.Close
        assertWhenCloseContract = Semantics.Assert Semantics.TrueObs $ Semantics.When [] distantFuture Semantics.Close
        whenCloseContract timeout = Semantics.When [] (POSIXTime timeout) Semantics.Close
        whenCloseContract' = Semantics.When [] distantFuture Semantics.Close
        whenWhenCloseContract timeout timeout' = Semantics.When [] (POSIXTime timeout) $ Semantics.When [] (POSIXTime timeout') Semantics.Close
        whenNotify = Semantics.When [Semantics.Case (Semantics.Notify Semantics.TrueObs) Semantics.Close] distantFuture Semantics.Close
        afterAssert = Semantics.Assert Semantics.TrueObs
    Hspec.QuickCheck.prop "valid slot interval for timed-out contract" \assets utxo address marloweParams state -> do
      -- The choice intervals for the tip, minimum time, and timeout overlap, so every ordering will occur.
      tipSlot' <- genTipSlot
      minTime <- genMinTime
      timeout <- genTimeout
      timeout' <- genTimeout
      -- Consider contracts with one or two timeouts that will occur, given the current time.
      marloweContract <- elements [whenCloseContract timeout, whenWhenCloseContract timeout timeout'] -- This contract can only time out.
      let tipSlot = Chain.SlotNo $ fromInteger tipSlot'
          tipTime = 1000 * tipSlot'
          marloweState = state{Semantics.minTime = POSIXTime minTime}
          datum = Semantics.MarloweData{..}
          marloweOutput = TransactionScriptOutput{..}
          result =
            runExcept $
              buildApplyInputsConstraints
                (const $ pure Nothing)
                systemStart
                eraHistory
                MarloweV1
                marloweOutput
                tipSlot
                emptyMarloweTransactionMetadata
                Nothing
                Nothing
                mempty
      pure
        . counterexample ("tipTime = " <> show tipTime)
        . counterexample ("minTime = " <> show minTime)
        . counterexample ("contract = " <> show marloweContract)
        . counterexample ("result = " <> show result)
        $ case result of
          Right _ ->
            counterexample "A valid transaction will occur if tip is not before the first timeout." $
              toSlot timeout <= tipSlot'
          Left (ApplyInputsConstraintsBuildupFailed (MarloweComputeTransactionFailed "TEUselessTransaction")) ->
            counterexample "A useless transaction will occur if the tip is before the timeout." $
              tipTime < timeout
          Left (ApplyInputsConstraintsBuildupFailed (MarloweComputeTransactionFailed message)) ->
            if "TEIntervalError (IntervalInPastError " `isPrefixOf` message
              then
                counterexample "The tip is in the past if the interval was in the past." $
                  tipTime < minTime
              else
                if "TEIntervalError (InvalidInterval " `isPrefixOf` message
                  then
                    counterexample "Rounding off causes the timeout to fall at the tip if the interval was invalid (effectively empty)." $
                      tipSlot' == toSlot timeout || marloweContract == whenWhenCloseContract timeout timeout' && tipSlot' == toSlot timeout'
                  else counterexample "Unexpected transaction failure" False
          Left _ ->
            counterexample "Unexpected transaction failure" False
    Hspec.QuickCheck.prop "valid slot interval for non-timed-out contract" \assets utxo address marloweParams state -> do
      -- The choice intervals for the tip, minimum time, and timeout overlap, so every ordering will occur.
      tipTime <- (1_000 *) <$> chooseInteger (0, 1_000) -- Choose the tip first.
      minTime <- chooseInteger (0, tipTime) -- Choose a minimum before the tip.
      timeout <- chooseInteger (tipTime + 1_000, tipTime + 1_000_000)
      let tipSlot = Chain.SlotNo $ fromInteger $ tipTime `div` 1_000
          marloweState = state{Semantics.minTime = POSIXTime minTime}
          marloweContract = afterAssert $ whenCloseContract timeout
          datum = Semantics.MarloweData{..}
          marloweOutput = TransactionScriptOutput{..}
          result =
            runExcept $
              buildApplyInputsConstraints
                (const $ pure Nothing)
                systemStart
                eraHistory
                MarloweV1
                marloweOutput
                tipSlot
                emptyMarloweTransactionMetadata
                Nothing
                Nothing
                mempty
      pure
        . counterexample ("tipTime = " <> show tipTime)
        . counterexample ("minTime = " <> show minTime)
        . counterexample ("timeout = " <> show timeout)
        . counterexample ("contract = " <> show marloweContract)
        . counterexample ("result = " <> show result)
        $ case result of
          Right _ -> counterexample "A valid transaction will occur if tip is before the first timeout." True
          Left _ -> counterexample "Unexpected transaction failure" False
    Hspec.QuickCheck.prop "respects client-specified slot interval" \assets utxo address marloweParams state -> do
      tipTime <- (1_000 *) <$> chooseInteger (0, 1_000) -- Choose the tip first.
      minTime <- chooseInteger (0, tipTime) -- Choose a minimum before the tip.
      lower <- oneof [pure Nothing, Just <$> chooseInteger (0, tipTime)] -- Choose a lower bound be not after the tip.
      upper <- oneof [pure Nothing, Just <$> chooseInteger (tipTime + 1_000, 2_000_000)] -- Choose an upper bound at least one slot after the tip.
      let tipSlot = Chain.SlotNo $ fromInteger $ tipTime `div` 1_000
          marloweContract = assertCloseContract
          marloweState = state{Semantics.minTime = 0}
          datum = Semantics.MarloweData{..}
          marloweOutput = TransactionScriptOutput{..}
          result =
            runExcept $
              buildApplyInputsConstraints
                (const $ pure Nothing)
                systemStart
                eraHistory
                MarloweV1
                marloweOutput
                tipSlot
                emptyMarloweTransactionMetadata
                (toUTC <$> lower)
                (toUTC <$> upper)
                mempty
      -- Simply make sure that the client's slot interval is not altered, aside from rounding to slot times.
      pure
        . counterexample ("minTime = " <> show minTime)
        . counterexample ("lower = " <> show lower)
        . counterexample ("tipTime = " <> show tipTime)
        . counterexample ("specified upper = " <> show upper)
        . counterexample ("specified result = " <> show result)
        $ case result of
          Right ((lower', upper', _, _), _) ->
            counterexample "Rounded specified bounds should match the computed bounds"
              . counterexample ("computed lower = " <> show (fromUTC lower'))
              . counterexample ("computed upper = " <> show (fromUTC upper'))
              $ maybe True ((== fromUTC lower') . toSecondFloor) lower -- The specified lower bound should be rounded down to the second/slot.
                && maybe True ((== fromUTC upper') . toSecondFloor) upper -- The specified upper bound should be rounded down to the second/slot.
          _ -> counterexample "Assert-close contract is valid" False
    Hspec.QuickCheck.prop "payment constraints" \assets utxo address marloweParams choices values -> do
      -- Create a bunch of payments.
      forAllShrink (listOf $ Semantics.Payment <$> arbitrary <*> arbitrary <*> arbitrary <*> chooseInteger (1, 1000)) shrink \payments ->
        do
          let makePayToAddress (Semantics.Payment _ (Semantics.Party (Semantics.Address network address')) token amount) =
                Map.singleton (toChainAddress network address') $ toChainAssets token amount
              makePayToAddress _ = mempty
              makePayToRole (Semantics.Payment _ (Semantics.Party (Semantics.Role name)) token amount) =
                Map.singleton (toChainRole marloweParams name) $ toChainAssets token amount
              makePayToRole _ = mempty
              makeAccount (Semantics.Payment account _ token amount) = Map.singleton (account, token) amount
              makePay (Semantics.Payment account payee token amount) = Semantics.Pay account payee token $ Semantics.Constant amount
              -- Fill the accounts with sufficient funds to make the payments.
              accounts = AM.fromList . Map.toList . Map.unionsWith (+) $ makeAccount <$> payments
              marloweState = Semantics.State accounts choices values $ POSIXTime 0
              -- Add all of the payments to the contract.
              marloweContract = foldr makePay assertWhenCloseContract payments
              datum = Semantics.MarloweData{..}
              marloweOutput = TransactionScriptOutput{..}
              result =
                runExcept $
                  buildApplyInputsConstraints
                    (const $ pure Nothing)
                    systemStart
                    eraHistory
                    MarloweV1
                    marloweOutput
                    (Chain.SlotNo 1_000_000)
                    emptyMarloweTransactionMetadata
                    Nothing
                    Nothing
                    mempty
              expectedPayToAddresses = Map.unionsWith (<>) $ makePayToAddress <$> payments -- This assumes that the semigroup for assets is correct.
              expectedPayToRoles = Map.unionsWith (<>) $ makePayToRole <$> payments -- This assumes that the semigroup for assets is correct.
          pure
            . counterexample ("contract = " <> show marloweContract)
            . counterexample ("result = " <> show result)
            . counterexample ("expected pays to addresses = " <> show expectedPayToAddresses)
            . counterexample ("expected pays to roles = " <> show expectedPayToRoles)
            $ case result of
              Right (_, TxConstraints{..}) ->
                counterexample "role and address payments are correct" $
                  payToAddresses == expectedPayToAddresses
                    && payToRoles == expectedPayToRoles
              Left _ ->
                counterexample "Unexpected transaction failure" False
          :: QuickCheck.Gen Property
    Hspec.QuickCheck.prop "input constraints" \assets utxo address marloweParams state inputs -> do
      tipTime <- (1_000 *) <$> chooseInteger (0, 1_000) -- Choose the tip first.
      minTime <- chooseInteger (0, tipTime) -- Choose a minimum before the tip.
      lower <- chooseInteger (0, tipTime) -- Choose a lower bound be not after the tip.
      upper <- chooseInteger (tipTime + 1_000, 2_000_000) -- Choose an upper bound at least one slot after the tip.
      closes <- arbitrary
      let tipSlot = Chain.SlotNo $ fromInteger $ tipTime `div` 1_000
          marloweState = state{Semantics.minTime = POSIXTime minTime}
          remainder = if closes then Semantics.Close else whenCloseContract'
          -- Create a contract with arbitrary inputs.
          marloweContract = afterAssert $ foldr (toContract . toAction) remainder inputs
          datum = Semantics.MarloweData{..}
          marloweOutput = TransactionScriptOutput{..}
          inputs' = Semantics.NormalInput <$> inputs
          result =
            runExcept $
              buildApplyInputsConstraints
                (const $ pure Nothing)
                systemStart
                eraHistory
                MarloweV1
                marloweOutput
                tipSlot
                emptyMarloweTransactionMetadata
                (Just $ toUTC lower)
                (Just $ toUTC upper)
                inputs'
      -- Make sure that the required inputs are present as a constraint.
      pure
        . counterexample ("minTime = " <> show minTime)
        . counterexample ("lower = " <> show lower)
        . counterexample ("tipTime = " <> show tipTime)
        . counterexample ("specified upper = " <> show upper)
        . counterexample ("specified result = " <> show result)
        . counterexample ("contract = " <> show marloweContract)
        . counterexample ("inputs = " <> show inputs)
        . counterexample ("result = " <> show result)
        $ case result of
          Right (_, TxConstraints{..}) ->
            counterexample "input is correct" $
              marloweInputConstraints == MarloweInput (toSlotNo lower) (toSlotNo upper) inputs'
          Left _ ->
            counterexample "Unexpected transaction failure" False
    Hspec.QuickCheck.prop "output constraints" \assets utxo address marloweParams state -> do
      closes <- arbitrary
      let marloweState = state{Semantics.minTime = 0}
          expectedContract = if closes then Semantics.Close else whenCloseContract'
          -- The contract just makes some payments, or waits.
          marloweContract = afterAssert expectedContract
          datum = Semantics.MarloweData{..}
          marloweOutput = TransactionScriptOutput{..}
          result =
            runExcept $
              buildApplyInputsConstraints
                (const $ pure Nothing)
                systemStart
                eraHistory
                MarloweV1
                marloweOutput
                (Chain.SlotNo 1_000_000)
                emptyMarloweTransactionMetadata
                Nothing
                Nothing
                mempty
          expectedAssets = fromPlutusValue . Semantics.totalBalance $ Semantics.accounts marloweState
          expectedDatum =
            datum
              { Semantics.marloweContract = expectedContract
              , Semantics.marloweState = marloweState{Semantics.minTime = 1_000_000_000}
              }
          -- There is no output if the contract closes, otherwise the assets and datum must be in the output.
          expectedOutput =
            if closes
              then MarloweOutputConstraintsNone
              else MarloweOutput expectedAssets expectedDatum
      pure
        . counterexample ("contract = " <> show marloweContract)
        . counterexample ("result = " <> show result)
        . counterexample ("expected output = " <> show expectedOutput)
        $ case result of
          Right ((_, _, Just output, _), TxConstraints{..}) ->
            counterexample "continuing output is correct" $
              marloweOutputConstraints == expectedOutput
                && output == (expectedAssets, expectedDatum)
          Right ((_, _, Nothing, _), TxConstraints{..}) ->
            counterexample "no continuing output is correct" $
              marloweOutputConstraints == expectedOutput
          Left _ ->
            counterexample "Unexpected transaction failure" False
    Hspec.QuickCheck.prop "metadata constraints" \assets utxo address marloweParams state metadata -> do
      let marloweState = state{Semantics.minTime = 0}
          marloweContract = assertWhenCloseContract
          datum = Semantics.MarloweData{..}
          marloweOutput = TransactionScriptOutput{..}
          result =
            runExcept $
              buildApplyInputsConstraints
                (const $ pure Nothing)
                systemStart
                eraHistory
                MarloweV1
                marloweOutput
                (Chain.SlotNo 1_000_000)
                metadata
                Nothing
                Nothing
                mempty
      -- Simply check if the arbitrary metadata is present in the constraints.
      pure
        . counterexample ("result = " <> show result)
        $ case result of
          Right (_, TxConstraints{..}) ->
            counterexample "metadata is preserved is correct" $
              metadataConstraints == metadata
          Left _ ->
            counterexample "Unexpected transaction failure" False
        :: QuickCheck.Gen Property
    Hspec.QuickCheck.prop "signature constraints" \assets utxo address marloweParams state inputs -> do
      let marloweState = state{Semantics.minTime = 0}
          -- Create a contract with arbitrary inputs.
          marloweContract = foldr (toContract . toAction) (afterAssert whenNotify) inputs
          datum = Semantics.MarloweData{..}
          marloweOutput = TransactionScriptOutput{..}
          result =
            runExcept $
              buildApplyInputsConstraints
                (const $ pure Nothing)
                systemStart
                eraHistory
                MarloweV1
                marloweOutput
                (Chain.SlotNo 1_000_000)
                emptyMarloweTransactionMetadata
                Nothing
                Nothing
                (Semantics.NormalInput <$> inputs)
          -- Determine what payment key hashes must be present.
          expectedPaymentKeyHashes = Set.unions $ toAddress <$> inputs
      pure
        . counterexample ("contract = " <> show marloweContract)
        . counterexample ("result = " <> show result)
        . counterexample ("expected payment key hashes = " <> show expectedPaymentKeyHashes)
        $ case result of
          Right (_, TxConstraints{..}) ->
            counterexample "signatures are present" $
              signatureConstraints == expectedPaymentKeyHashes
          Left _ ->
            counterexample "Unexpected transaction failure" False
        :: QuickCheck.Gen Property
    Hspec.QuickCheck.prop "role constraints" \assets utxo address marloweParams state inputs -> do
      let marloweState = state{Semantics.minTime = 0}
          -- Create a contract with arbitrary inputs.
          marloweContract = foldr (toContract . toAction) (afterAssert whenNotify) inputs
          datum = Semantics.MarloweData{..}
          marloweOutput = TransactionScriptOutput{..}
          result =
            runExcept $
              buildApplyInputsConstraints
                (const $ pure Nothing)
                systemStart
                eraHistory
                MarloweV1
                marloweOutput
                (Chain.SlotNo 1_000_000)
                emptyMarloweTransactionMetadata
                Nothing
                Nothing
                (Semantics.NormalInput <$> inputs)
          -- Determine what roles must be present.
          expectedRoles = Set.unions $ toRole marloweParams <$> inputs
      pure
        . counterexample ("contract = " <> show marloweContract)
        . counterexample ("result = " <> show result)
        . counterexample ("expected roles = " <> show expectedRoles)
        $ case result of
          Right (_, TxConstraints{..}) ->
            counterexample "roles are present" $
              roleTokenConstraints == if Set.null expectedRoles then RoleTokenConstraintsNone else SpendRoleTokens expectedRoles
          Left _ ->
            counterexample "Unexpected transaction failure" False
        :: QuickCheck.Gen Property
    it "[BUG] PLT-8793: Outputs correct number of role tokens" do
      let inputs = [V1.IDeposit (V1.Role "Receiver") (V1.Role "Sender") (V1.Token "" "") 5_000_000_000]
          previousOutput :: TransactionScriptOutput 'V1
          previousOutput =
            TransactionScriptOutput
              { address = fromJust $ fromBech32 "addr_test1wrv9l2du900ajl27hk79u07xda68vgfugrppkua5zftlp8g0l9djk"
              , assets =
                  Chain.Assets
                    { ada = 2_000_000
                    , tokens = mempty
                    }
              , utxo = Chain.TxOutRef "4240f052b02797b0ba51d549fa0a02aad5b377382682215d36e3bd999f0117c5" 1
              , datum =
                  V1.MarloweData
                    { marloweParams = V1.MarloweParams "177246d7a843c1bd64d87e152608147257f657a0525f273208e9d173"
                    , marloweContract =
                        V1.When
                          [ V1.Case
                              ( V1.Deposit
                                  (V1.Role "Receiver")
                                  (V1.Role "Sender")
                                  (V1.Token "" "")
                                  (V1.Constant 5000000000)
                              )
                              (V1.When [V1.Case (V1.Notify V1.TrueObs) V1.Close] 1700772840000 V1.Close)
                          ]
                          1700772840000
                          V1.Close
                    , marloweState =
                        V1.State
                          { accounts =
                              AM.fromList
                                [
                                  (
                                    ( uncurry V1.Address $
                                        fromJust $
                                          V1.deserialiseAddressBech32
                                            "addr_test1qq9prvx8ufwutkwxx9cmmuuajaqmjqwujqlp9d8pvg6gupcvluken35ncjnu0puetf5jvttedkze02d5kf890kquh60slacjyp"
                                    , V1.Token "" ""
                                    )
                                  , 2_000_000
                                  )
                                ]
                          , choices = AM.empty
                          , boundValues = AM.empty
                          , minTime = 0
                          }
                    }
              }
          result =
            runExcept $
              snd
                <$> buildApplyInputsConstraints
                  (const $ pure Nothing)
                  systemStart
                  eraHistory
                  MarloweV1
                  previousOutput
                  (Chain.SlotNo 1_000_000)
                  emptyMarloweTransactionMetadata
                  Nothing
                  Nothing
                  (Semantics.NormalInput <$> inputs)
      constraints <- either (fail . show) pure result
      let marloweContext =
            MarloweContext
              { scriptOutput = Just previousOutput
              , marloweAddress = fromJust $ fromBech32 "addr_test1wrv9l2du900ajl27hk79u07xda68vgfugrppkua5zftlp8g0l9djk"
              , payoutAddress = Chain.Address "7010ec7e02d25f5836b3e1098e0d4d8389e71d7a97a57aa737adc1d1fa"
              , marloweScriptHash = "d85fa9bc2bdfd97d5ebdbc5e3fc66f7476213c40c21b73b41257f09d"
              , payoutScriptHash = "10ec7e02d25f5836b3e1098e0d4d8389e71d7a97a57aa737adc1d1fa"
              , marloweScriptUTxO =
                  ReferenceScriptUtxo
                    { txOutRef = "c59678b6892ba0fbeeaaec22d4cbde17026ff614ed47cea02c47752e5853ebc8#1"
                    , txOut =
                        Chain.TransactionOutput
                          { address = fromJust $ fromBech32 "addr_test1vz53mawjqyzly4zdd7yr97s4wkef0y3jtzfzvtc2xr4rv4skmmd6n"
                          , assets = Chain.Assets 53_461_240 mempty
                          , datum = Nothing
                          , datumHash = Nothing
                          }
                    , script = node812V1MarloweScript
                    }
              , payoutScriptUTxO =
                  ReferenceScriptUtxo
                    { txOutRef = "c59678b6892ba0fbeeaaec22d4cbde17026ff614ed47cea02c47752e5853ebc8#2"
                    , txOut =
                        Chain.TransactionOutput
                          { address = fromJust $ fromBech32 "addr_test1vrkl3n9lfzzrkh7xsvg3apja297f5hsvdl8kawa8a5868mqecyjgc"
                          , assets = Chain.Assets 12_249_020 mempty
                          , datum = Nothing
                          , datumHash = Nothing
                          }
                    , script = node812V1PayoutScript
                    }
              }
          mkWalletOutput ada tokens =
            Chain.TransactionOutput
              { address =
                  fromJust $
                    fromBech32
                      "addr_test1qq9prvx8ufwutkwxx9cmmuuajaqmjqwujqlp9d8pvg6gupcvluken35ncjnu0puetf5jvttedkze02d5kf890kquh60slacjyp"
              , assets = Chain.Assets ada $ Chain.Tokens $ Map.fromList do
                  (quantity, policy, Chain.Base16 token) <- tokens
                  pure (Chain.AssetId policy $ Chain.TokenName token, quantity)
              , datumHash = Nothing
              , datum = Nothing
              }
          walletContext =
            WalletContext
              { availableUtxos =
                  Chain.UTxOs $
                    Map.fromList
                      [
                        ( "09ba5a347fc9d9290865ef0fac634988106db9cc7caf54e7c2578bcdcd327a14#1"
                        , mkWalletOutput 1172320 [(1, "28a9d34903993185694b03d50246a4c16c2b505616b6036222e1499b", "4e69752053656c6c6572")]
                        )
                      ,
                        ( "09ba5a347fc9d9290865ef0fac634988106db9cc7caf54e7c2578bcdcd327a14#2"
                        , mkWalletOutput 1172320 [(1, "8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d", "4265617247617264656e")]
                        )
                      , ("11650cb360dc061358464906a266bc1f7368949a9697801f810343d8507f4e7a#4", mkWalletOutput 2000000 [])
                      ,
                        ( "28baba9a4f7642f8315ce89146457228e36e672a9de192154363386ffe11b895#3"
                        , mkWalletOutput
                            1935190
                            [ (1, "285e6ade72ec0f11965c3c077e1c68d1e21ec3eecbd0da32948ca222", "546872656164")
                            , (1, "82e4a99993453eab26698daa558da44d88caa12a796c864149570ac1", "53656c6c6572")
                            , (1, "82e4a99993453eab26698daa558da44d88caa12a796c864149570ac1", "546872656164")
                            , (1, "87c3d209c1ff0e25eed4766472dfb6884dd1a7a64c0c1f6bd5fd0ef7", "616e617263686f73796e646963616c")
                            , (1, "87c3d209c1ff0e25eed4766472dfb6884dd1a7a64c0c1f6bd5fd0ef7", "6d61726c6f77652d6f7261636c65")
                            , (1, "87c3d209c1ff0e25eed4766472dfb6884dd1a7a64c0c1f6bd5fd0ef7", "777368616b65737065617265")
                            , (997, "8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d", "4265617247617264656e")
                            , (1, "8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d", "632e6d61726c6f7765")
                            ]
                        )
                      ,
                        ( "30fe07b15cd809ef34bdf90d84ca673644454258b0ca79631d6b0c67d2e339fd#1"
                        , mkWalletOutput 1168010 [(1, "7c5d08a9c1951ebaa3f515a23200db8d579898bace6ca50d55490751", "4e6975204275796572")]
                        )
                      ,
                        ( "30fe07b15cd809ef34bdf90d84ca673644454258b0ca79631d6b0c67d2e339fd#2"
                        , mkWalletOutput 1172320 [(1, "8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d", "4265617247617264656e")]
                        )
                      , ("35119844de7f629474fb68c8ad099529a8c7922aa46c48e81350d24ed79a41b6#1", mkWalletOutput 2000000 [])
                      ,
                        ( "3c3ff0811369ceeab1303b323672497158c10847bd94958cca250a9e0a64fb1b#1"
                        , mkWalletOutput 1146460 [(1, "87c3d209c1ff0e25eed4766472dfb6884dd1a7a64c0c1f6bd5fd0ef7", "666261636f6e")]
                        )
                      ,
                        ( "3f6a0e686cb1612f616b6bd0fce89090bf0f9aa6bfe4969a02ab94f88f752ff1#3"
                        , mkWalletOutput 2000000 [(1, "28a9d34903993185694b03d50246a4c16c2b505616b6036222e1499b", "4e697520436f6e7472616374")]
                        )
                      , ("4240f052b02797b0ba51d549fa0a02aad5b377382682215d36e3bd999f0117c5#0", mkWalletOutput 23184951 [])
                      ,
                        ( "4240f052b02797b0ba51d549fa0a02aad5b377382682215d36e3bd999f0117c5#3"
                        , mkWalletOutput 1155080 [(1, "177246d7a843c1bd64d87e152608147257f657a0525f273208e9d173", "53656e646572")]
                        )
                      ,
                        ( "505dc761e42bb1a7c09e3d7cc5257416ca4f67120d5fa842264782dc1ad350bc#1"
                        , mkWalletOutput 1172320 [(1, "87c3d209c1ff0e25eed4766472dfb6884dd1a7a64c0c1f6bd5fd0ef7", "66756e6374696f6e616c6c79")]
                        )
                      , ("5625d9c7fc3dc260d185fbc08afc167d028f99b68a271fa210fa5453763a0302#4", mkWalletOutput 2000000 [])
                      ,
                        ( "562824d40bee708014e607003590e9a8e108c99d52f30826a2b7d35b5cc9c0f9#1"
                        , mkWalletOutput 1168010 [(1, "b09a293a0b25384026f7adf873e06cbfdaa01daef267a49fb33dd897", "4e6975204275796572")]
                        )
                      ,
                        ( "82f95c3963a748fa6fe069062544b341ec6e31f1cf3eabf75985c88fd8db158e#1"
                        , mkWalletOutput 1172320 [(1, "7c5d08a9c1951ebaa3f515a23200db8d579898bace6ca50d55490751", "4e69752053656c6c6572")]
                        )
                      ,
                        ( "8596492cd2eca8385863231eeaa5e261cbc88275e5b5c271bce8dfd829dda734#1"
                        , mkWalletOutput 1155080 [(1, "87c3d209c1ff0e25eed4766472dfb6884dd1a7a64c0c1f6bd5fd0ef7", "6d68657262657274")]
                        )
                      ,
                        ( "96c204856fc7c5e51bc4ba60e45ff3c40653c760ad4f25677ca8cbc57ab1521c#2"
                        , mkWalletOutput 1189560 [(1, "f24109f3c799f2061938ea6bc5fc6c39e081296910e062e56ddc2228", "746563686e6f73796e646963616c")]
                        )
                      ,
                        ( "9bf7b0fab9f9e560fd585973c5a3acf1c208bd9a66c7b6cc8b4c066c1adf9365#1"
                        , mkWalletOutput 1172320 [(1, "b09a293a0b25384026f7adf873e06cbfdaa01daef267a49fb33dd897", "4e69752053656c6c6572")]
                        )
                      ,
                        ( "aa0e76faf3032c4b90bf2a981817989c42b4f0e12a3d0f6b749f6fe203f0afce#1"
                        , mkWalletOutput 1150770 [(1, "87c3d209c1ff0e25eed4766472dfb6884dd1a7a64c0c1f6bd5fd0ef7", "65646576657265")]
                        )
                      ,
                        ( "ae6e50699314c1fe022ec47db6f6f5bc052e9a011862f19a3ebfa96c544ac747#1"
                        , mkWalletOutput 1159390 [(1, "87c3d209c1ff0e25eed4766472dfb6884dd1a7a64c0c1f6bd5fd0ef7", "66626561756d6f6e74")]
                        )
                      , ("aef5b62f60c46819c776356198a1472acd267c5be14a798cc473ba118750ed24#2", mkWalletOutput 2000000 [])
                      ,
                        ( "b266865a97cef99146c61bca96ff3c3b5bae0b9a46982640c128fb90a248b33b#3"
                        , mkWalletOutput 2000000 [(1, "7c5d08a9c1951ebaa3f515a23200db8d579898bace6ca50d55490751", "4e697520436f6e7472616374")]
                        )
                      ,
                        ( "b3e4a0547abaf51e46816fcf402bbf682f5a6caac4e4079fb9cb65e3ada1d38d#1"
                        , mkWalletOutput 1155080 [(1, "87c3d209c1ff0e25eed4766472dfb6884dd1a7a64c0c1f6bd5fd0ef7", "6a77656273746572")]
                        )
                      , ("b666e5f6d235981ab6d711711c860e098a08a608020d950117c934b2834e53c4#0", mkWalletOutput 146528488242 [])
                      , ("b666e5f6d235981ab6d711711c860e098a08a608020d950117c934b2834e53c4#1", mkWalletOutput 2000000 [])
                      ,
                        ( "b7352637d367d1dad2a3a8e049b4e08492b50b2d167c7b60e2ef1ebbd67abf04#1"
                        , mkWalletOutput
                            1995530
                            [ (1, "87c3d209c1ff0e25eed4766472dfb6884dd1a7a64c0c1f6bd5fd0ef7", "6563617279")
                            , (1991, "8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d", "476c6f6265")
                            , (2015000, "8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d", "484f534b59")
                            , (1015, "8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d", "5377616e")
                            , (1, "8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d", "64656c6179666565646261636b")
                            , (1, "8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d", "662e62656175666f6e74")
                            , (1, "8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d", "6761696e")
                            , (1, "8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d", "6c656761746f")
                            , (1, "8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d", "6e6f7465")
                            , (1, "8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d", "7265736f6e616e6365")
                            , (1, "8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d", "726f6f6d")
                            , (1, "8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d", "736f756e64")
                            , (1, "8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d", "7370656564")
                            , (1, "8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d", "766f77656c")
                            , (250000000, "9772ff715b691c0444f333ba1db93b055c0864bec48fff92d1f2a7fe", "446a65645f746573744d6963726f555344")
                            ]
                        )
                      , ("b7352637d367d1dad2a3a8e049b4e08492b50b2d167c7b60e2ef1ebbd67abf04#2", mkWalletOutput 16567348 [])
                      , ("b98464f6a1eeded278c911f0baa86c015cae9f539c3b8bcba34318451815d6e2#0", mkWalletOutput 3446503894 [])
                      ,
                        ( "e0421270883f718ae9d6e8cb82adc6a0fb8cf079491ff67faeea2d65c88d1e31#1"
                        , mkWalletOutput
                            1271450
                            [ (1, "87c3d209c1ff0e25eed4766472dfb6884dd1a7a64c0c1f6bd5fd0ef7", "616e617263686f73796e64692d64616f")
                            , (1, "87c3d209c1ff0e25eed4766472dfb6884dd1a7a64c0c1f6bd5fd0ef7", "636d61726c6f7765")
                            , (1, "87c3d209c1ff0e25eed4766472dfb6884dd1a7a64c0c1f6bd5fd0ef7", "6a6c756d6c6579")
                            ]
                        )
                      , ("ecd9821ee33d490847484ad54ca209ad03721009bfece621d01293232cbe5909#0", mkWalletOutput 8004204456 [])
                      ,
                        ( "f474411379cdcc45c14484798e2f035b0bd4ca78ef35a14ef07c39935d0a36b2#3"
                        , mkWalletOutput 2000000 [(1, "b09a293a0b25384026f7adf873e06cbfdaa01daef267a49fb33dd897", "4e697520436f6e7472616374")]
                        )
                      , ("f52718d219e1d90d318b68ae8d51092c66c0c676f6a925e249df5e5efe099f0c#0", mkWalletOutput 27386015026 [])
                      , ("fa0d85e4fce4f993075800e8c0b5f286611cb6566a3a42c5b4ccdee95e90ff2c#1", mkWalletOutput 2000000 [])
                      , ("fde1e7735f001959798c1f9318d4f247fcc4a6d14942c8b487c36a45bf2a6443#0", mkWalletOutput 300098570 [])
                      ]
              , collateralUtxos = mempty
              , changeAddress =
                  fromJust $
                    fromBech32
                      "addr_test1qq9prvx8ufwutkwxx9cmmuuajaqmjqwujqlp9d8pvg6gupcvluken35ncjnu0puetf5jvttedkze02d5kf890kquh60slacjyp"
              }
          helpersContext =
            HelpersContext
              { currentHelperScripts = mempty
              , helperPolicyId = "177246d7a843c1bd64d87e152608147257f657a0525f273208e9d173"
              , helperScriptStates = mempty
              }
          solveResult =
            solveConstraints
              systemStart
              (toLedgerEpochInfo eraHistory)
              protocolTestnet
              ReferenceTxInsScriptsInlineDatumsInBabbageEra
              MarloweV1
              (Left marloweContext)
              walletContext
              helpersContext
              constraints
      TxBody TxBodyContent{..} <- either (fail . show) pure solveResult
      let roleTokenOutputCount = getSum $ flip foldMap txOuts \case
            TxOut _ (TxOutValue MultiAssetInBabbageEra value) _ _ ->
              Sum $
                selectAsset value $
                  AssetId
                    (fromJust $ toCardanoPolicyId "177246d7a843c1bd64d87e152608147257f657a0525f273208e9d173")
                    (AssetName "Sender")
            TxOut _ (TxOutAdaOnly era _) _ _ -> case era of {}
      roleTokenOutputCount `shouldBe` 1
