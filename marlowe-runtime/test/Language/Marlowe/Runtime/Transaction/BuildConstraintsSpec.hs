{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Language.Marlowe.Runtime.Transaction.BuildConstraintsSpec
  ( spec
  ) where

import Cardano.Api (ConsensusMode(..), EraHistory(EraHistory), SlotNo(SlotNo))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Function (on)
import Data.List (isPrefixOf)
import qualified Data.List.NonEmpty as NE
import Data.Map (Map)
import qualified Data.Map as Map
import Data.SOP.Strict (K(..), NP(..))
import qualified Data.Set as Set
import Data.Time (UTCTime, nominalDiffTimeToSeconds, secondsToNominalDiffTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import GHC.Generics (Generic)
import qualified Language.Marlowe.Core.V1.Semantics as Semantics
import qualified Language.Marlowe.Core.V1.Semantics.Types as Semantics
import qualified Language.Marlowe.Core.V1.Semantics.Types.Address as Semantics
import Language.Marlowe.Runtime.ChainSync.Api (Lovelace, toUTxOsList)
import qualified Language.Marlowe.Runtime.ChainSync.Api as Chain
import Language.Marlowe.Runtime.Core.Api
  ( Contract
  , Datum
  , MarloweTransactionMetadata
  , MarloweVersion(..)
  , MarloweVersionTag(..)
  , TransactionScriptOutput(..)
  , emptyMarloweTransactionMetadata
  )
import qualified Language.Marlowe.Runtime.Core.Api as Core.Api
import Language.Marlowe.Runtime.Plutus.V2.Api (fromPlutusValue, toAssetId)
import Language.Marlowe.Runtime.Transaction.Api
  (ApplyInputsConstraintsBuildupError(..), ApplyInputsError(..), CreateError, RoleTokensConfig(..), mkMint, unMint)
import qualified Language.Marlowe.Runtime.Transaction.Api as Transaction.Api
import Language.Marlowe.Runtime.Transaction.BuildConstraints (buildApplyInputsConstraints, buildCreateConstraints)
import qualified Language.Marlowe.Runtime.Transaction.BuildConstraints as BuildConstraints
import Language.Marlowe.Runtime.Transaction.Constraints
  ( MarloweInputConstraints(..)
  , MarloweOutputConstraints(..)
  , RoleTokenConstraints(..)
  , TxConstraints(..)
  , WalletContext(..)
  )
import qualified Language.Marlowe.Runtime.Transaction.Constraints as TxConstraints
import Language.Marlowe.Runtime.Transaction.ConstraintsSpec (genRole)
import Ouroboros.Consensus.BlockchainTime (RelativeTime(..), SystemStart(..), mkSlotLength)
import Ouroboros.Consensus.HardFork.History
  (Bound(..), EraEnd(..), EraParams(..), EraSummary(..), SafeZone(..), mkInterpreter, summaryWithExactly)
import Ouroboros.Consensus.Util.Counting (Exactly(..))
import Plutus.V1.Ledger.Api (Address(Address), Credential(PubKeyCredential), PubKeyHash(PubKeyHash), fromBuiltin)
import Plutus.V1.Ledger.Time (POSIXTime(POSIXTime))
import qualified PlutusTx.AssocMap as AM
import Spec.Marlowe.Semantics.Arbitrary ()
import Test.Hspec (Spec, shouldBe)
import qualified Test.Hspec as Hspec
import qualified Test.Hspec.QuickCheck as Hspec.QuickCheck
import Test.QuickCheck
  ( Arbitrary(..)
  , Property
  , chooseInteger
  , counterexample
  , discard
  , elements
  , forAllShrink
  , genericShrink
  , listOf
  , listOf1
  , oneof
  , suchThat
  , (===)
  )
import qualified Test.QuickCheck as QuickCheck
import Test.QuickCheck.Instances ()

byteStringGen :: QuickCheck.Gen ByteString
byteStringGen = BS.pack <$> QuickCheck.arbitrary

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
    let result = extractMarloweAssets <$> runBuildCreateConstraints args
    in case version args of
      MarloweV1 -> result === (Right $ Just $ Chain.Assets (minAda args) mempty)
      :: Property
  Hspec.QuickCheck.prop "sends minted role tokens" \(SomeCreateArgs args) ->
    case roleTokensConfig args of
      RoleTokensMint mint ->
        let
          result = extractSentRoleTokens <$> runBuildCreateConstraints args
          expected = fst <$> unMint mint
        in case version args of
          MarloweV1 -> result === Right expected
          :: Property
      _ -> discard
  Hspec.QuickCheck.prop "total balance == marlowe output assets" \(SomeCreateArgs args) ->
    let
      result = runBuildCreateConstraints args
      mDatum = extractMarloweDatum <$> result
      mAssets = extractMarloweAssets <$> result
    in case version args of
      MarloweV1 -> (fmap (fromPlutusValue . Semantics.totalBalance . Semantics.accounts . Semantics.marloweState) <$> mDatum) === mAssets
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

  where
    emptyStateProp :: (Eq a, Show a) => String -> (CreateArgs 'V1 -> Semantics.State -> a) -> Spec
    emptyStateProp name f = Hspec.QuickCheck.prop name \(SomeCreateArgs args) ->
      let result = extractMarloweDatum <$> runBuildCreateConstraints args
      in case version args of
          MarloweV1 -> on (===) ((fmap . fmap) (f args))
            (fmap Semantics.marloweState <$> result)
            (Right (Just $ Semantics.emptyState 0))
        :: Property

extractMarloweDatum :: TxConstraints v -> Maybe (Datum v)
extractMarloweDatum TxConstraints{..} = case marloweOutputConstraints of
  MarloweOutput _ datum -> Just datum
  _ -> Nothing

extractSentRoleTokens :: TxConstraints v -> Map Chain.TokenName Chain.Address
extractSentRoleTokens TxConstraints{..} = case roleTokenConstraints of
  MintRoleTokens _ _ distribution -> Map.mapKeys Chain.tokenName distribution
  _ -> mempty

extractMarloweAssets :: TxConstraints v -> Maybe Chain.Assets
extractMarloweAssets TxConstraints{..} = case marloweOutputConstraints of
  MarloweOutput assets _ -> Just assets
  _ -> Nothing

runBuildCreateConstraints :: CreateArgs v -> Either (CreateError v) (TxConstraints v)
runBuildCreateConstraints CreateArgs{..} =
  snd <$> buildCreateConstraints version walletContext roleTokensConfig metadata minAda contract

data CreateArgs v = CreateArgs
  { version :: MarloweVersion v
  , walletContext :: WalletContext
  , roleTokensConfig :: RoleTokensConfig
  , metadata :: MarloweTransactionMetadata
  , minAda :: Lovelace
  , contract :: Contract v
  } deriving (Generic)

instance Arbitrary (CreateArgs 'V1) where
  arbitrary = CreateArgs MarloweV1
    <$> arbitrary `suchThat` notEmptyWalletContext
    <*> arbitrary
    <*> arbitrary
    <*> (Chain.Lovelace <$> arbitrary)
    <*> arbitrary
  shrink args@CreateArgs{..} = concat
    [ [ args { walletContext = x } | x <- shrink walletContext ]
    , [ args { roleTokensConfig = x } | x <- shrink roleTokensConfig ]
    , [ args { metadata = x } | x <- shrink metadata ]
    , [ args { contract = x } | x <- shrink contract ]
    ]

instance Arbitrary WalletContext where
  arbitrary = WalletContext <$> arbitrary <*> arbitrary <*> arbitrary
  shrink = genericShrink

instance Arbitrary RoleTokensConfig where
  arbitrary = oneof
    [ pure RoleTokensNone
    , RoleTokensUsePolicy . Chain.PolicyId <$> byteStringGen
    , RoleTokensMint . mkMint . NE.fromList <$> listOf1 ((,) <$> genRole <*> ((,Nothing) <$> arbitrary))
    ]
  shrink = \case
    RoleTokensNone -> []
    RoleTokensUsePolicy _ -> [RoleTokensNone]
    RoleTokensMint _ -> [RoleTokensNone]

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
  Hspec.QuickCheck.prop "implements Marlowe V1" do
    tokenName <- Chain.TokenName <$> byteStringGen
    policyId <- Chain.PolicyId <$> byteStringGen

    let assetId :: Chain.AssetId
        assetId = Chain.AssetId policyId tokenName

        actual :: Either (Transaction.Api.WithdrawError 'Core.Api.V1) (TxConstraints 'Core.Api.V1)
        actual = BuildConstraints.buildWithdrawConstraints Core.Api.MarloweV1 assetId

        expected :: Either (Transaction.Api.WithdrawError 'Core.Api.V1) (TxConstraints 'Core.Api.V1)
        expected = Right $ TxConstraints
          { marloweInputConstraints = TxConstraints.MarloweInputConstraintsNone
          , payoutInputConstraints = Set.singleton assetId
          , roleTokenConstraints = TxConstraints.SpendRoleTokens $ Set.singleton assetId
          , payToAddresses = Map.empty
          , payToRoles = Map.empty
          , marloweOutputConstraints = TxConstraints.MarloweOutputConstraintsNone
          , signatureConstraints = Set.empty
          , metadataConstraints = emptyMarloweTransactionMetadata
          }

    pure $ actual `shouldBe` expected

buildApplyInputsConstraintsSpec :: Spec
buildApplyInputsConstraintsSpec =
  Hspec.describe "buildApplyInputsConstraints" do
    -- TODO: Break up this preface to utility functions when additional properties are tested.
    let
      -- System start and era history are reusable across tests.
      systemStart = SystemStart $ posixSecondsToUTCTime 0  -- Without loss of generality.
      eraParams = EraParams
        { eraEpochSize = 1
        , eraSlotLength = mkSlotLength 1
        , eraSafeZone = UnsafeIndefiniteSafeZone
        }
      oneSecondBound i = Bound
        { boundTime = RelativeTime $ fromInteger i
        , boundSlot = fromInteger i
        , boundEpoch = fromInteger i
        }
      oneSecondEraSummary i = EraSummary
        { eraStart = oneSecondBound i
        , eraEnd = EraEnd $ oneSecondBound $ i + 1
        , eraParams
        }
      unboundedEraSummary i = EraSummary
        { eraStart = oneSecondBound i
        , eraEnd = EraUnbounded
        , eraParams
        }
      eraHistory =
        EraHistory CardanoMode
          . mkInterpreter
          . summaryWithExactly
          $ Exactly
          $  K (oneSecondEraSummary 0) -- Byron lasted 1 second
          :* K (oneSecondEraSummary 1) -- Shelley lasted 1 second
          :* K (oneSecondEraSummary 2) -- Allegra lasted 1 second
          :* K (oneSecondEraSummary 3) -- Mary lasted 1 second
          :* K (oneSecondEraSummary 4) -- Alonzo lasted 1 second
          :* K (unboundedEraSummary 5) -- Babbage never ends
          :* Nil
      -- Important note: these slot computations cannot be used generally, but are specifically tailored
      -- to the contrived era history and system start used for this test case.
      genTipSlot = chooseInteger (9, 20) -- Make sure the tip is in the Babbage era.
      genMinTime = oneof
                   [
                     (1000 *) <$> chooseInteger (6, 20)  -- Even seconds, so there is a chance of collision.
                   , chooseInteger (6000, 20000)         -- Milliseconds, so there is rounding off.
                   ]
      genTimeout = oneof
                   [
                     (1000 *) <$> chooseInteger (6, 20)  -- Even seconds, so there is a chance of collision.
                   , chooseInteger (6000, 20000)         -- Milliseconds, so there is rounding off.
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
      let
        tipSlot = Chain.SlotNo $ fromInteger tipSlot'
        tipTime = 1000 * tipSlot'
        marloweState = state {Semantics.minTime = POSIXTime minTime}
        datum = Semantics.MarloweData{..}
        marloweOutput = TransactionScriptOutput{..}
        result =
          buildApplyInputsConstraints
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
              counterexample "A valid transaction will occur if tip is not before the first timeout."
                $ toSlot timeout <= tipSlot'
            Left (ApplyInputsConstraintsBuildupFailed (MarloweComputeTransactionFailed "TEUselessTransaction")) ->
              counterexample "A useless transaction will occur if the tip is before the timeout."
                $ tipTime < timeout
            Left (ApplyInputsConstraintsBuildupFailed (MarloweComputeTransactionFailed message)) ->
              if "TEIntervalError (IntervalInPastError " `isPrefixOf` message
                then counterexample "The tip is in the past if the interval was in the past."
                  $ tipTime < minTime
                else if "TEIntervalError (InvalidInterval " `isPrefixOf` message
                  then counterexample "Rounding off causes the timeout to fall at the tip if the interval was invalid (effectively empty)."
                    $ tipSlot' == toSlot timeout || marloweContract == whenWhenCloseContract timeout timeout' && tipSlot' == toSlot timeout'
                else counterexample "Unexpected transaction failure" False
            Left _ ->
              counterexample "Unexpected transaction failure" False
    Hspec.QuickCheck.prop "valid slot interval for non-timed-out contract" \assets utxo address marloweParams state -> do
      -- The choice intervals for the tip, minimum time, and timeout overlap, so every ordering will occur.
      tipTime <- (1_000 *) <$> chooseInteger (0, 1_000)                                   -- Choose the tip first.
      minTime <- chooseInteger (0, tipTime)                                               -- Choose a minimum before the tip.
      timeout <- chooseInteger (tipTime + 1_000, tipTime + 1_000_000)
      let
        tipSlot = Chain.SlotNo $ fromInteger $ tipTime `div` 1_000
        marloweState = state {Semantics.minTime = POSIXTime minTime}
        marloweContract = afterAssert $ whenCloseContract timeout
        datum = Semantics.MarloweData{..}
        marloweOutput = TransactionScriptOutput{..}
        result =
          buildApplyInputsConstraints
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
      tipTime <- (1_000 *) <$> chooseInteger (0, 1_000)                                   -- Choose the tip first.
      minTime <- chooseInteger (0, tipTime)                                               -- Choose a minimum before the tip.
      lower <- oneof [pure Nothing, Just <$> chooseInteger (0, tipTime)]                  -- Choose a lower bound be not after the tip.
      upper <- oneof [pure Nothing, Just <$> chooseInteger (tipTime + 1_000, 2_000_000)]  -- Choose an upper bound at least one slot after the tip.
      let
        tipSlot = Chain.SlotNo $ fromInteger $ tipTime `div` 1_000
        marloweContract = assertCloseContract
        marloweState = state {Semantics.minTime = 0}
        datum = Semantics.MarloweData{..}
        marloweOutput = TransactionScriptOutput{..}
        result =
          buildApplyInputsConstraints
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
            Right ((lower', upper', _), _) ->
              counterexample "Rounded specified bounds should match the computed bounds"
                . counterexample ("computed lower = " <> show (fromUTC lower'))
                . counterexample ("computed upper = " <> show (fromUTC upper'))
                $  maybe True ((== fromUTC lower') . toSecondFloor) lower  -- The specified lower bound should be rounded down to the second/slot.
                && maybe True ((== fromUTC upper') . toSecondFloor) upper  -- The specified upper bound should be rounded down to the second/slot.
            _ -> counterexample "Assert-close contract is valid" False
    Hspec.QuickCheck.prop "payment constraints" \assets utxo address marloweParams choices values -> do
      -- Create a bunch of payments.
      forAllShrink (listOf $ Semantics.Payment <$> arbitrary <*> arbitrary <*> arbitrary <*> chooseInteger (1, 1000)) shrink \payments -> do
        let
          makePayToAddress (Semantics.Payment _ (Semantics.Party (Semantics.Address network address')) token amount) =
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
            buildApplyInputsConstraints
              systemStart
              eraHistory
              MarloweV1
              marloweOutput
              (Chain.SlotNo 1_000_000)
              emptyMarloweTransactionMetadata
              Nothing
              Nothing
              mempty
          expectedPayToAddresses = Map.unionsWith (<>) $ makePayToAddress <$> payments  -- This assumes that the semigroup for assets is correct.
          expectedPayToRoles     = Map.unionsWith (<>) $ makePayToRole    <$> payments  -- This assumes that the semigroup for assets is correct.
        pure
          . counterexample ("contract = " <> show marloweContract)
          . counterexample ("result = " <> show result)
          . counterexample ("expected pays to addresses = " <> show expectedPayToAddresses)
          . counterexample ("expected pays to roles = " <> show expectedPayToRoles)
          $ case result of
              Right (_, TxConstraints{..}) ->
                counterexample "role and address payments are correct"
                  $  payToAddresses == expectedPayToAddresses
                  && payToRoles     == expectedPayToRoles
              Left _ ->
                counterexample "Unexpected transaction failure" False
        :: QuickCheck.Gen Property
    Hspec.QuickCheck.prop "input constraints" \assets utxo address marloweParams state inputs -> do
      tipTime <- (1_000 *) <$> chooseInteger (0, 1_000)    -- Choose the tip first.
      minTime <- chooseInteger (0, tipTime)                -- Choose a minimum before the tip.
      lower <- chooseInteger (0, tipTime)                  -- Choose a lower bound be not after the tip.
      upper <- chooseInteger (tipTime + 1_000, 2_000_000)  -- Choose an upper bound at least one slot after the tip.
      closes <- arbitrary
      let
        tipSlot = Chain.SlotNo $ fromInteger $ tipTime `div` 1_000
        marloweState = state {Semantics.minTime = POSIXTime minTime}
        remainder = if closes then Semantics.Close else whenCloseContract'
        -- Create a contract with arbitrary inputs.
        marloweContract = afterAssert $ foldr (toContract . toAction) remainder inputs
        datum = Semantics.MarloweData{..}
        marloweOutput = TransactionScriptOutput{..}
        inputs' = Semantics.NormalInput <$> inputs
        result =
          buildApplyInputsConstraints
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
              counterexample "input is correct"
                $ marloweInputConstraints == MarloweInput (toSlotNo lower) (toSlotNo upper) inputs'
            Left _ ->
              counterexample "Unexpected transaction failure" False
    Hspec.QuickCheck.prop "output constraints" \assets utxo address marloweParams state -> do
      closes <- arbitrary
      let
        marloweState = state {Semantics.minTime = 0}
        expectedContract = if closes then Semantics.Close else whenCloseContract'
        -- The contract just makes some payments, or waits.
        marloweContract = afterAssert expectedContract
        datum = Semantics.MarloweData{..}
        marloweOutput = TransactionScriptOutput{..}
        result =
          buildApplyInputsConstraints
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
        expectedDatum = datum {Semantics.marloweContract = expectedContract, Semantics.marloweState = marloweState {Semantics.minTime = 1_000_000_000}}
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
            Right ((_, _, Just output), TxConstraints{..}) ->
              counterexample "continuing output is correct"
                $ marloweOutputConstraints == expectedOutput
                && output == (expectedAssets, expectedDatum)
            Right ((_, _, Nothing), TxConstraints{..}) ->
              counterexample "no continuing output is correct"
                $ marloweOutputConstraints == expectedOutput
            Left _ ->
              counterexample "Unexpected transaction failure" False
    Hspec.QuickCheck.prop "metadata constraints" \assets utxo address marloweParams state metadata -> do
      let
        marloweState = state {Semantics.minTime = 0}
        marloweContract = assertWhenCloseContract
        datum = Semantics.MarloweData{..}
        marloweOutput = TransactionScriptOutput{..}
        result =
          buildApplyInputsConstraints
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
              counterexample "metadata is preserved is correct"
                $ metadataConstraints == metadata
            Left _ ->
              counterexample "Unexpected transaction failure" False
        :: QuickCheck.Gen Property
    Hspec.QuickCheck.prop "signature constraints" \assets utxo address marloweParams state inputs -> do
      let
        marloweState = state {Semantics.minTime = 0}
        -- Create a contract with arbitrary inputs.
        marloweContract = foldr (toContract . toAction) (afterAssert whenNotify) inputs
        datum = Semantics.MarloweData{..}
        marloweOutput = TransactionScriptOutput{..}
        result =
          buildApplyInputsConstraints
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
              counterexample "signatures are present"
                $ signatureConstraints == expectedPaymentKeyHashes
            Left _ ->
              counterexample "Unexpected transaction failure" False
        :: QuickCheck.Gen Property
    Hspec.QuickCheck.prop "role constraints" \assets utxo address marloweParams state inputs -> do
      let
        marloweState = state {Semantics.minTime = 0}
        -- Create a contract with arbitrary inputs.
        marloweContract = foldr (toContract . toAction) (afterAssert whenNotify) inputs
        datum = Semantics.MarloweData{..}
        marloweOutput = TransactionScriptOutput{..}
        result =
          buildApplyInputsConstraints
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
              counterexample "roles are present"
                $ roleTokenConstraints == if Set.null expectedRoles then RoleTokenConstraintsNone else SpendRoleTokens expectedRoles
            Left _ ->
              counterexample "Unexpected transaction failure" False
        :: QuickCheck.Gen Property
