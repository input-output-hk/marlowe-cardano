{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Language.Marlowe.Runtime.Transaction.BuildConstraintsSpec (
  spec,
) where

import Cardano.Api (EraHistory (EraHistory), SlotNo (SlotNo))
import qualified Cardano.Api as C
import Control.Monad.Trans.Except (runExcept, runExceptT)
import Data.Function (on)
import Data.Functor ((<&>))
import Data.Functor.Identity (Identity (..))
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Map.NonEmpty as NEMap
import Data.Maybe (fromJust, fromMaybe, maybeToList)
import Data.SOP.BasicFunctors (K (..))
import Data.SOP.Counting (Exactly (..))
import Data.SOP.Strict (NP (..))
import qualified Data.Set as Set
import Data.Time (UTCTime, nominalDiffTimeToSeconds, secondsToNominalDiffTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import Data.Traversable (for)
import GHC.Generics (Generic)
import qualified Language.Marlowe.Core.V1.Semantics as Semantics
import qualified Language.Marlowe.Core.V1.Semantics.Types as Semantics
import qualified Language.Marlowe.Core.V1.Semantics.Types.Address as Semantics
import Language.Marlowe.Runtime.Cardano.Api (fromCardanoPolicyId, toCardanoPlutusScript)
import Language.Marlowe.Runtime.ChainSync.Api (
  Lovelace,
  PlutusScript (..),
  Quantity (Quantity),
  TxOutAssets (unTxOutAssets),
  mkTxOutAssets,
  toUTxOsList,
 )
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
import Language.Marlowe.Runtime.Core.ScriptRegistry (HelperScript (..))
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
  AdjustMinUTxO (..),
  buildApplyInputsConstraints,
  buildCreateConstraints,
  safeLovelace,
 )
import qualified Language.Marlowe.Runtime.Transaction.BuildConstraints as BuildConstraints
import Language.Marlowe.Runtime.Transaction.Constraints (
  Distribution (..),
  MarloweInputConstraints (..),
  MarloweOutputConstraints (..),
  PayoutContext (..),
  RoleTokenConstraints (..),
  TxConstraints (..),
  WalletContext (..),
 )
import qualified Language.Marlowe.Runtime.Transaction.Constraints as TxConstraints
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
import Test.Hspec (Spec, shouldBe)
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

type TestEra = C.BabbageEra

babbageEraOnwardsTest :: C.BabbageEraOnwards TestEra
babbageEraOnwardsTest = C.BabbageEraOnwardsBabbage

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
    let result = fmap Chain.ada . extractMarloweAssets' <$> runBuildCreateConstraints args
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
        mAssets = extractMarloweAssets' <$> result
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
          assets = fmap unTxOutAssets . extractMarloweAssets <$> constraints
          result = (Map.lookup threadTokenAssetId . Chain.unTokens . Chain.tokens =<<) <$> assets
       in case version args of
            MarloweV1 ->
              counterexample (show threadTokenAssetId)
                . counterexample (show constraints)
                . counterexample (show assets)
                $ result === Right (Just $ Quantity 1)
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
  Map.filter (> mempty) . \case
    RoleTokensNone -> mempty
    RoleTokensUsePolicy _ dist -> flattenMap dist
    RoleTokensMint (Mint mint) -> flattenMap $ NEMap.toMap $ NEMap.toMap . roleTokenRecipients <$> mint

flattenMap :: Map a (Map b c) -> Map (a, b) c
flattenMap abc = Map.fromDistinctAscList do
  (a, bc) <- Map.toAscList abc
  (b, c) <- Map.toAscList bc
  pure ((a, b), c)

extractSentRoleTokens
  :: TxConstraints TestEra v
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

extractMarloweDatum :: TxConstraints TestEra v -> Maybe (Datum v)
extractMarloweDatum TxConstraints{..} = case marloweOutputConstraints of
  MarloweOutput _ datum -> Just datum
  _ -> Nothing

extractMarloweAssets :: TxConstraints TestEra v -> Maybe Chain.TxOutAssets
extractMarloweAssets TxConstraints{..} = case marloweOutputConstraints of
  MarloweOutput assets _ -> Just assets
  _ -> Nothing

extractMarloweAssets' :: TxConstraints TestEra v -> Maybe Chain.Assets
extractMarloweAssets' = fmap unTxOutAssets . extractMarloweAssets

runBuildCreateConstraints :: CreateArgs v -> Either CreateError (TxConstraints TestEra v)
runBuildCreateConstraints CreateArgs{..} = do
  let adjustMinUTxO = AdjustMinUTxO id
  snd
    <$> runIdentity
      ( buildCreateConstraints
          -- Since we don't actually run the script, we can just return empty bytes
          (\_ _ -> pure testMintingValidator)
          babbageEraOnwardsTest
          version
          walletContext
          threadName
          roleTokensConfig
          metadata
          minAda
          mempty
          adjustMinUTxO
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
      <*> ((<> safeLovelace) . Chain.Lovelace <$> arbitrary)
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
      let actual :: Either Transaction.Api.WithdrawError (TxConstraints TestEra 'Core.Api.V1)
          actual = runIdentity $ runExceptT $ snd <$> BuildConstraints.buildWithdrawConstraints payoutContext Core.Api.MarloweV1 payouts

          expected :: Either Transaction.Api.WithdrawError (TxConstraints TestEra 'Core.Api.V1)
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
          EraHistory
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
        mkTxOutAssets' = fromMaybe mempty . mkTxOutAssets
        -- Chain conversions.
        toChainAddress = (Chain.Address .) . Semantics.serialiseAddress
        toChainAssets (Semantics.Token "" "") amount = mkTxOutAssets' $ Chain.Assets (Chain.Lovelace $ fromInteger amount) mempty
        toChainAssets (Semantics.Token currency name) amount =
          mkTxOutAssets' $
            Chain.Assets mempty . Chain.Tokens $
              Map.singleton (toAssetId currency name) (Chain.Quantity $ fromInteger amount)
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
          Left (ApplyInputsConstraintsBuildupFailed (MarloweComputeTransactionFailed Semantics.TEUselessTransaction)) ->
            counterexample "A useless transaction will occur if the tip is before the timeout." $
              tipTime < timeout
          Left (ApplyInputsConstraintsBuildupFailed (MarloweComputeTransactionFailed (Semantics.TEIntervalError intervalError))) ->
            case intervalError of
              (Semantics.IntervalInPastError _ _) -> counterexample "The tip is in the past if the interval was in the past." $ tipTime < minTime
              (Semantics.InvalidInterval _) ->
                counterexample "Roundoff causes the timeout to fall at the tip if the interval was invalid (effectively empty)." $
                  tipSlot' == toSlot timeout || marloweContract == whenWhenCloseContract timeout timeout' && tipSlot' == toSlot timeout'
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
          expectedAssets = mkTxOutAssets' $ fromPlutusValue . Semantics.totalBalance $ Semantics.accounts marloweState
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
