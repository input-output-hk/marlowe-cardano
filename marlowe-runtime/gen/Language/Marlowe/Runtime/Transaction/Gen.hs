{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Language.Marlowe.Runtime.Transaction.Gen where

import Cardano.Api (
  BabbageEraOnwards (..),
  IsShelleyBasedEra,
  ShelleyBasedEra (ShelleyBasedEraBabbage, ShelleyBasedEraConway),
  shelleyBasedEra,
 )
import Control.Applicative (liftA2)
import qualified Data.ByteString.Char8 as BS
import Data.Foldable (fold)
import qualified Data.List.NonEmpty as NE
import Data.Map.NonEmpty (NEMap)
import qualified Data.Map.NonEmpty as NEMap
import Data.Semigroup (Semigroup (..))
import Language.Marlowe.Runtime.ChainSync.Gen ()
import qualified Language.Marlowe.Runtime.Core.Api as Core
import Language.Marlowe.Runtime.Core.Gen (ArbitraryMarloweVersion)
import Language.Marlowe.Runtime.Core.ScriptRegistry (HelperScript)
import Language.Marlowe.Runtime.History.Gen ()
import Language.Marlowe.Runtime.Transaction.Api
import qualified Language.Marlowe.Runtime.Transaction.Api as ContractCreatedInEra (ContractCreatedInEra (..))
import qualified Language.Marlowe.Runtime.Transaction.Api as InputsAppliedInEra (InputsAppliedInEra (..))
import qualified Language.Marlowe.Runtime.Transaction.Api as WithdrawTxInEra (WithdrawTxInEra (..))
import Network.HTTP.Media (MediaType, (//))
import Network.Protocol.Codec.Spec (Variations (..), varyAp)
import Network.Protocol.Job.Types (ArbitraryCommand (..), CommandVariations (..), SomeTag (..))
import qualified Network.URI
import qualified Network.URI as Network
import Spec.Marlowe.Semantics.Arbitrary ()
import Test.Gen.Cardano.Api.Typed (genTx, genTxBody)
import Test.QuickCheck hiding (shrinkMap)
import Test.QuickCheck.Hedgehog (hedgehog)
import Test.QuickCheck.Instances ()

instance Arbitrary MediaType where
  arbitrary = do
    let stringGen = do
          n <- chooseInt (1, 127)
          BS.pack <$> vectorOf n (elements ['a' .. 'z'])
    liftA2 (//) stringGen stringGen

instance Arbitrary Network.URIAuth where
  arbitrary =
    Network.URIAuth
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
  shrink = genericShrink

instance Arbitrary Network.URI where
  arbitrary = do
    uriScheme <- do
      c <- charLetterGen
      fmap (c :) $ oneof [pure "", listOf $ oneof [charLetterGen, charNumberGen]]

    uriAuthority <-
      oneof
        [ pure Nothing
        , do
            uriUserInfo <- oneof [pure "", listOf specialCharGen]
            uriRegName <- oneof [pure "", listOf specialCharGen]
            uriPort <- oneof [pure "", listOf charNumberGen]
            pure $ Just $ Network.URIAuth{..}
        ]

    uriPath <- oneof [pure "", ('/' :) <$> listOf specialCharGen]
    uriQuery <- oneof [pure "", listOf1 specialCharGen]
    uriFragment <- oneof [pure "", listOf1 specialCharGen]

    pure $ Network.URI.rectify $ Network.URI{..}
    where
      specialCharGen :: Gen Char
      specialCharGen = oneof [charLetterGen, charNumberGen, elements ['.', '-', '_', '=', ';']]

      charLetterGen :: Gen Char
      charLetterGen = elements $ ['a' .. 'z'] <> ['A' .. 'Z']

      charNumberGen :: Gen Char
      charNumberGen = elements ['0' .. '9']

instance Arbitrary WalletAddresses where
  arbitrary = WalletAddresses <$> arbitrary <*> arbitrary <*> arbitrary
  shrink = genericShrink

instance Arbitrary NFTMetadataFile where
  arbitrary =
    NFTMetadataFile
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
  shrink = genericShrink

instance Arbitrary RoleTokenMetadata where
  arbitrary =
    RoleTokenMetadata
      <$> frequency [(1, pure ""), (9, arbitrary)]
      <*> arbitrary
      <*> frequency [(1, pure Nothing), (9, arbitrary)]
      <*> frequency [(1, pure (Just "")), (1, pure Nothing), (8, arbitrary)]
      <*> arbitrary
      <*> arbitrary
  shrink = genericShrink

instance Arbitrary HelperScript where
  arbitrary = elements [minBound .. maxBound]
  shrink = genericShrink

instance Arbitrary Destination where
  arbitrary =
    frequency
      [ (15, ToAddress <$> arbitrary)
      , (1, ToScript <$> arbitrary)
      ]
  shrink = genericShrink

instance (Ord k, Arbitrary k, Arbitrary v) => Arbitrary (NEMap k v) where
  arbitrary = NEMap.insertMap <$> arbitrary <*> arbitrary <*> arbitrary
  shrink (NEMap.deleteFindMin -> ((k, v), m)) =
    fold
      [ NEMap.insertMap <$> shrink k <*> pure v <*> pure m
      , NEMap.insertMap k <$> shrink v <*> pure m
      , NEMap.insertMap k v <$> shrink m
      ]

instance Arbitrary MintRole where
  arbitrary = MintRole <$> arbitrary <*> arbitrary
  shrink = genericShrink

instance Arbitrary Mint where
  arbitrary = Mint <$> arbitrary
  shrink = genericShrink

instance Arbitrary RoleTokensConfig where
  arbitrary =
    frequency
      [ (1, pure RoleTokensNone)
      , (10, RoleTokensUsePolicy <$> arbitrary <*> arbitrary)
      , (10, RoleTokensMint <$> arbitrary)
      ]
  shrink = \case
    RoleTokensNone -> []
    RoleTokensUsePolicy policy dist -> uncurry RoleTokensUsePolicy <$> shrink (policy, dist)
    RoleTokensMint mint -> RoleTokensMint <$> shrink mint

instance Arbitrary SubmitStatus where
  arbitrary = elements [Submitting, Accepted]
  shrink = genericShrink

instance Arbitrary LoadMarloweContextError where
  arbitrary =
    oneof
      [ pure LoadMarloweContextErrorNotFound
      , LoadMarloweContextErrorVersionMismatch <$> arbitrary
      , pure LoadMarloweContextToCardanoError
      , MarloweScriptNotPublished <$> arbitrary
      , PayoutScriptNotPublished <$> arbitrary
      , ExtractCreationError <$> arbitrary
      , ExtractMarloweTransactionError <$> arbitrary
      ]
  shrink = genericShrink

instance Arbitrary LoadHelpersContextError where
  arbitrary = HelperScriptNotFoundInRegistry <$> arbitrary
  shrink = genericShrink

instance Arbitrary CoinSelectionError where
  arbitrary =
    oneof
      [ NoCollateralFound <$> arbitrary
      , InsufficientLovelace <$> arbitrary <*> arbitrary
      , InsufficientTokens <$> arbitrary
      ]

instance Arbitrary ConstraintError where
  arbitrary =
    oneof
      [ MintingUtxoNotFound <$> arbitrary
      , RoleTokenNotFound <$> arbitrary
      , pure ToCardanoError
      , pure MissingMarloweInput
      , PayoutNotFound <$> arbitrary
      , CalculateMinUtxoFailed <$> arbitrary
      , CoinSelectionFailed <$> arbitrary
      , BalancingError <$> arbitrary
      , InvalidPayoutDatum <$> arbitrary <*> arbitrary
      , InvalidPayoutScriptAddress <$> arbitrary <*> arbitrary
      , pure MarloweInputInWithdraw
      , pure MarloweOutputInWithdraw
      , pure PayoutOutputInWithdraw
      , pure PayoutInputInCreateOrApply
      ]
  shrink = genericShrink

instance Arbitrary CreateBuildupError where
  arbitrary =
    oneof
      [ pure MintingUtxoSelectionFailed
      , AddressDecodingFailed <$> arbitrary
      , MintingScriptDecodingFailed <$> arbitrary
      ]
  shrink = genericShrink

instance Arbitrary CreateError where
  arbitrary =
    oneof
      [ CreateConstraintError <$> arbitrary
      , CreateEraUnsupported <$> arbitrary
      , CreateLoadMarloweContextFailed <$> arbitrary
      , CreateBuildupFailed <$> arbitrary
      , pure CreateToCardanoError
      ]
  shrink = genericShrink

instance Arbitrary ApplyInputsConstraintsBuildupError where
  arbitrary =
    oneof
      [ MarloweComputeTransactionFailed <$> arbitrary
      , pure UnableToDetermineTransactionTimeout
      ]
  shrink = genericShrink

instance Arbitrary ApplyInputsError where
  arbitrary =
    oneof
      [ ApplyInputsEraUnsupported <$> arbitrary
      , ApplyInputsConstraintError <$> arbitrary
      , pure ScriptOutputNotFound
      , ApplyInputsLoadMarloweContextFailed <$> arbitrary
      , ApplyInputsLoadHelpersContextFailed <$> arbitrary
      , ApplyInputsConstraintsBuildupFailed <$> arbitrary
      , SlotConversionFailed <$> arbitrary
      , pure TipAtGenesis
      , ValidityLowerBoundTooHigh <$> arbitrary <*> arbitrary
      ]
  shrink = genericShrink

instance Arbitrary WithdrawError where
  arbitrary =
    oneof
      [ WithdrawConstraintError <$> arbitrary
      , WithdrawEraUnsupported <$> arbitrary
      , WithdrawLoadHelpersContextFailed <$> arbitrary
      , pure EmptyPayouts
      ]
  shrink = genericShrink

instance Arbitrary SubmitError where
  arbitrary =
    oneof
      [ SubmitException <$> arbitrary
      , SubmitFailed <$> arbitrary
      , pure TxDiscarded
      ]
  shrink = genericShrink

instance (ArbitraryMarloweVersion v) => Arbitrary (ContractCreated v) where
  arbitrary =
    oneof
      [ ContractCreated BabbageEraOnwardsBabbage <$> arbitrary
      , ContractCreated BabbageEraOnwardsConway <$> arbitrary
      ]
  shrink (ContractCreated BabbageEraOnwardsBabbage created) =
    ContractCreated BabbageEraOnwardsBabbage <$> shrink created
  shrink (ContractCreated BabbageEraOnwardsConway created) =
    ContractCreated BabbageEraOnwardsConway <$> shrink created

instance (ArbitraryMarloweVersion v, IsShelleyBasedEra era) => Arbitrary (ContractCreatedInEra era v) where
  arbitrary =
    ContractCreatedInEra
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> pure Core.marloweVersion
      <*> arbitrary
      <*> arbitrary
      <*> hedgehog (genTxBody shelleyBasedEra)
      <*> arbitrary
  shrink ContractCreatedInEra{..} =
    fold
      [ [ContractCreatedInEra{..}{ContractCreatedInEra.metadata = metadata'} | metadata' <- shrink metadata]
      , [ContractCreatedInEra{..}{ContractCreatedInEra.datum = datum'} | datum' <- shrink datum]
      , [ContractCreatedInEra{..}{ContractCreatedInEra.assets = assets'} | assets' <- shrink assets]
      , [ContractCreatedInEra{..}{ContractCreatedInEra.safetyErrors = safetyErrors'} | safetyErrors' <- shrink safetyErrors]
      ]

instance (ArbitraryMarloweVersion v) => Arbitrary (InputsApplied v) where
  arbitrary =
    oneof
      [ InputsApplied BabbageEraOnwardsBabbage <$> arbitrary
      , InputsApplied BabbageEraOnwardsConway <$> arbitrary
      ]
  shrink (InputsApplied BabbageEraOnwardsBabbage created) =
    InputsApplied BabbageEraOnwardsBabbage <$> shrink created
  shrink (InputsApplied BabbageEraOnwardsConway created) =
    InputsApplied BabbageEraOnwardsConway <$> shrink created

instance (ArbitraryMarloweVersion v, IsShelleyBasedEra era) => Arbitrary (InputsAppliedInEra era v) where
  arbitrary =
    InputsAppliedInEra Core.marloweVersion
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> hedgehog (genTxBody shelleyBasedEra)
  shrink InputsAppliedInEra{..} =
    fold
      [ [InputsAppliedInEra{..}{InputsAppliedInEra.metadata = metadata'} | metadata' <- shrink metadata]
      , [InputsAppliedInEra{..}{InputsAppliedInEra.input = input'} | input' <- shrink input]
      , [InputsAppliedInEra{..}{InputsAppliedInEra.output = output'} | output' <- shrink output]
      , [InputsAppliedInEra{..}{InputsAppliedInEra.inputs = inputs'} | inputs' <- shrink inputs]
      ]

instance (ArbitraryMarloweVersion v) => Arbitrary (WithdrawTx v) where
  arbitrary =
    oneof
      [ WithdrawTx BabbageEraOnwardsBabbage <$> arbitrary
      , WithdrawTx BabbageEraOnwardsConway <$> arbitrary
      ]
  shrink (WithdrawTx BabbageEraOnwardsBabbage created) =
    WithdrawTx BabbageEraOnwardsBabbage <$> shrink created
  shrink (WithdrawTx BabbageEraOnwardsConway created) =
    WithdrawTx BabbageEraOnwardsConway <$> shrink created

instance (ArbitraryMarloweVersion v, IsShelleyBasedEra era) => Arbitrary (WithdrawTxInEra era v) where
  arbitrary =
    WithdrawTxInEra Core.marloweVersion
      <$> arbitrary
      <*> hedgehog (genTxBody shelleyBasedEra)
  shrink WithdrawTxInEra{..} = [WithdrawTxInEra{..}{WithdrawTxInEra.inputs = inputs'} | inputs' <- shrink inputs]

instance Arbitrary BurnTx where
  arbitrary =
    oneof
      [ BurnTx BabbageEraOnwardsBabbage <$> arbitrary
      , BurnTx BabbageEraOnwardsConway <$> arbitrary
      ]
  shrink (BurnTx BabbageEraOnwardsBabbage created) =
    BurnTx BabbageEraOnwardsBabbage <$> shrink created
  shrink (BurnTx BabbageEraOnwardsConway created) =
    BurnTx BabbageEraOnwardsConway <$> shrink created

instance (IsShelleyBasedEra era) => Arbitrary (BurnTxInEra era) where
  arbitrary = BurnTxInEra <$> arbitrary <*> hedgehog (genTxBody shelleyBasedEra)
  shrink BurnTxInEra{..} =
    [BurnTxInEra{..}{burnedTokens = burnedTokens'} | burnedTokens' <- shrink burnedTokens]

instance Arbitrary Account where
  arbitrary =
    oneof
      [ RoleAccount <$> arbitrary
      , AddressAccount <$> arbitrary
      ]
  shrink = genericShrink

instance Arbitrary RoleTokenFilter where
  arbitrary = sized \case
    0 -> frequency leaves
    size -> frequency $ leaves <> nodes size
    where
      leaves =
        [ (1, pure RoleTokenFilterAny)
        , (1, pure RoleTokenFilterNone)
        , (5, MkRoleTokenFilterByContracts <$> arbitrary)
        , (5, MkRoleTokenFilterByPolicyIds <$> arbitrary)
        , (5, MkRoleTokenFilterByTokens <$> arbitrary)
        ]
      nodes size =
        [ (5, resize (size `div` 2) $ MkRoleTokensOr <$> arbitrary <*> arbitrary)
        , (5, resize (size `div` 2) $ MkRoleTokensAnd <$> arbitrary <*> arbitrary)
        ]
  shrink = genericShrink

instance Arbitrary BurnError where
  arbitrary =
    frequency
      [ (5, BurnRolesActive <$> arbitrary)
      , (1, pure BurnNoTokens)
      , (3, BurnBalancingError <$> arbitrary)
      ]
  shrink = genericShrink

instance ArbitraryCommand MarloweTxCommand where
  arbitraryTag =
    elements
      [ SomeTag $ TagCreate Core.MarloweV1
      , SomeTag $ TagApplyInputs Core.MarloweV1
      , SomeTag $ TagWithdraw Core.MarloweV1
      , SomeTag TagBurn
      , SomeTag TagSubmit
      ]
  arbitraryCmd = \case
    TagCreate Core.MarloweV1 ->
      Create
        <$> arbitrary
        <*> pure Core.MarloweV1
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
    TagApplyInputs Core.MarloweV1 ->
      ApplyInputs Core.MarloweV1
        <$> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
    TagWithdraw Core.MarloweV1 ->
      Withdraw Core.MarloweV1
        <$> arbitrary
        <*> arbitrary
    TagBurn -> Burn <$> arbitrary <*> arbitrary
    TagSubmit ->
      oneof
        [ Submit BabbageEraOnwardsBabbage <$> hedgehog (genTx ShelleyBasedEraBabbage)
        , Submit BabbageEraOnwardsConway <$> hedgehog (genTx ShelleyBasedEraConway)
        ]
  arbitraryJobId = \case
    TagCreate Core.MarloweV1 -> Nothing
    TagApplyInputs Core.MarloweV1 -> Nothing
    TagWithdraw Core.MarloweV1 -> Nothing
    TagBurn -> Nothing
    TagSubmit -> Just $ JobIdSubmit <$> arbitrary
  arbitraryStatus = \case
    TagCreate Core.MarloweV1 -> Nothing
    TagApplyInputs Core.MarloweV1 -> Nothing
    TagWithdraw Core.MarloweV1 -> Nothing
    TagBurn -> Nothing
    TagSubmit -> Just arbitrary
  arbitraryErr = \case
    TagCreate Core.MarloweV1 -> Just arbitrary
    TagApplyInputs Core.MarloweV1 -> Just arbitrary
    TagWithdraw Core.MarloweV1 -> Just arbitrary
    TagBurn -> Just arbitrary
    TagSubmit -> Just arbitrary
  arbitraryResult = \case
    TagCreate Core.MarloweV1 -> arbitrary
    TagApplyInputs Core.MarloweV1 -> arbitrary
    TagWithdraw Core.MarloweV1 -> arbitrary
    TagBurn -> arbitrary
    TagSubmit -> arbitrary
  shrinkCommand = \case
    Create staking Core.MarloweV1 wallet thread roleConfig meta minAda state contract ->
      concat
        [ Create
            <$> shrink staking
            <*> pure Core.MarloweV1
            <*> pure wallet
            <*> pure thread
            <*> pure roleConfig
            <*> pure meta
            <*> pure minAda
            <*> pure state
            <*> pure contract
        , Create staking Core.MarloweV1
            <$> shrink wallet
            <*> pure thread
            <*> pure roleConfig
            <*> pure meta
            <*> pure minAda
            <*> pure state
            <*> pure contract
        , Create staking Core.MarloweV1 wallet
            <$> shrink thread
            <*> pure roleConfig
            <*> pure meta
            <*> pure minAda
            <*> pure state
            <*> pure contract
        , Create staking Core.MarloweV1 wallet thread
            <$> shrink roleConfig
            <*> pure meta
            <*> pure minAda
            <*> pure state
            <*> pure contract
        , Create staking Core.MarloweV1 wallet thread roleConfig
            <$> shrink meta
            <*> pure minAda
            <*> pure state
            <*> pure contract
        , Create staking Core.MarloweV1 wallet thread roleConfig meta
            <$> shrink minAda
            <*> pure state
            <*> pure contract
        , Create staking Core.MarloweV1 wallet thread roleConfig meta minAda
            <$> shrink state
            <*> pure contract
        , Create staking Core.MarloweV1 wallet thread roleConfig meta minAda state
            <$> shrink contract
        ]
    ApplyInputs Core.MarloweV1 wallet contractId meta minValid maxValid inputs ->
      concat
        [ ApplyInputs Core.MarloweV1
            <$> shrink wallet
            <*> pure contractId
            <*> pure meta
            <*> pure minValid
            <*> pure maxValid
            <*> pure inputs
        , ApplyInputs Core.MarloweV1 wallet
            <$> shrink contractId
            <*> pure meta
            <*> pure minValid
            <*> pure maxValid
            <*> pure inputs
        , ApplyInputs Core.MarloweV1 wallet contractId
            <$> shrink meta
            <*> pure minValid
            <*> pure maxValid
            <*> pure inputs
        , ApplyInputs Core.MarloweV1 wallet contractId meta
            <$> shrink minValid
            <*> pure maxValid
            <*> pure inputs
        , ApplyInputs Core.MarloweV1 wallet contractId meta minValid
            <$> shrink maxValid
            <*> pure inputs
        , ApplyInputs Core.MarloweV1 wallet contractId meta minValid maxValid
            <$> shrink inputs
        ]
    Withdraw Core.MarloweV1 wallet payouts ->
      (Withdraw Core.MarloweV1 <$> shrink wallet <*> pure payouts)
        <> (Withdraw Core.MarloweV1 wallet <$> shrink payouts)
    Burn wallet tokenFilter ->
      (Burn <$> shrink wallet <*> pure tokenFilter)
        <> (Burn wallet <$> shrink tokenFilter)
    Submit _ _ -> []
  shrinkJobId = \case
    JobIdSubmit txId -> JobIdSubmit <$> shrink txId
  shrinkErr = \case
    TagCreate Core.MarloweV1 -> shrink
    TagApplyInputs Core.MarloweV1 -> shrink
    TagWithdraw Core.MarloweV1 -> shrink
    TagBurn -> shrink
    TagSubmit -> shrink
  shrinkResult = \case
    TagCreate Core.MarloweV1 -> shrink
    TagApplyInputs Core.MarloweV1 -> shrink
    TagWithdraw Core.MarloweV1 -> shrink
    TagBurn -> shrink
    TagSubmit -> shrink
  shrinkStatus = \case
    TagCreate Core.MarloweV1 -> \case {}
    TagApplyInputs Core.MarloweV1 -> \case {}
    TagWithdraw Core.MarloweV1 -> \case {}
    TagBurn -> \case {}
    TagSubmit -> shrink

instance CommandVariations MarloweTxCommand where
  tags =
    NE.fromList
      [ SomeTag $ TagCreate Core.MarloweV1
      , SomeTag $ TagApplyInputs Core.MarloweV1
      , SomeTag $ TagWithdraw Core.MarloweV1
      , SomeTag TagSubmit
      ]
  cmdVariations = \case
    TagCreate Core.MarloweV1 ->
      Create
        <$> variations
          `varyAp` variations
          `varyAp` variations
          `varyAp` variations
          `varyAp` variations
          `varyAp` variations
          `varyAp` variations
          `varyAp` variations
          `varyAp` variations
    TagApplyInputs Core.MarloweV1 ->
      ApplyInputs Core.MarloweV1
        <$> variations
          `varyAp` variations
          `varyAp` variations
          `varyAp` variations
          `varyAp` variations
          `varyAp` variations
    TagWithdraw Core.MarloweV1 ->
      Withdraw Core.MarloweV1 <$> variations `varyAp` variations
    TagBurn ->
      Burn <$> variations `varyAp` variations
    TagSubmit ->
      sconcat $
        NE.fromList
          [ Submit BabbageEraOnwardsBabbage <$> variations
          , Submit BabbageEraOnwardsConway <$> variations
          ]
  jobIdVariations = \case
    TagCreate Core.MarloweV1 -> []
    TagApplyInputs Core.MarloweV1 -> []
    TagWithdraw Core.MarloweV1 -> []
    TagBurn -> []
    TagSubmit -> NE.toList $ JobIdSubmit <$> variations
  statusVariations = \case
    TagCreate Core.MarloweV1 -> []
    TagApplyInputs Core.MarloweV1 -> []
    TagWithdraw Core.MarloweV1 -> []
    TagBurn -> []
    TagSubmit -> NE.toList variations
  errVariations = \case
    TagCreate Core.MarloweV1 -> NE.toList variations
    TagApplyInputs Core.MarloweV1 -> NE.toList variations
    TagWithdraw Core.MarloweV1 -> NE.toList variations
    TagBurn -> NE.toList variations
    TagSubmit -> NE.toList variations
  resultVariations = \case
    TagCreate Core.MarloweV1 -> variations
    TagApplyInputs Core.MarloweV1 -> variations
    TagWithdraw Core.MarloweV1 -> variations
    TagBurn -> variations
    TagSubmit -> variations
