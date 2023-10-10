{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Language.Marlowe.Runtime.Transaction.Gen where

import Cardano.Api (CardanoEra (..), IsCardanoEra, cardanoEra)
import Cardano.Api.Shelley (
  ReferenceTxInsScriptsInlineDatumsSupportedInEra (..),
 )
import Control.Applicative (liftA2)
import qualified Data.ByteString.Char8 as BS
import Data.Foldable (fold)
import qualified Data.List.NonEmpty as NE
import Data.Semigroup (Semigroup (..))
import Language.Marlowe.Runtime.ChainSync.Gen ()
import qualified Language.Marlowe.Runtime.Core.Api as Core
import Language.Marlowe.Runtime.Core.Gen (ArbitraryMarloweVersion)
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

  shrink = genericShrink

instance Arbitrary WalletAddresses where
  arbitrary = WalletAddresses <$> arbitrary <*> arbitrary <*> arbitrary
  shrink = genericShrink

instance Arbitrary NFTMetadataFile where
  arbitrary =
    NFTMetadataFile
      <$> arbitrary
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
  shrink = genericShrink

instance Arbitrary HelperScript where
  arbitrary = elements [minBound .. maxBound]
  shrink = genericShrink

instance Arbitrary Destination where
  arbitrary =
    frequency
      [ (30, ToAddress <$> arbitrary)
      , (2, pure ToSelf)
      , (1, ToScript <$> arbitrary)
      ]
  shrink (ToAddress address) = ToAddress <$> genericShrink address
  shrink ToSelf = []
  shrink (ToScript script) = ToScript <$> genericShrink script

instance Arbitrary Mint where
  arbitrary = mkMint <$> arbitrary
  shrink = genericShrink

instance Arbitrary RoleTokensConfig where
  arbitrary =
    frequency
      [ (10, pure RoleTokensNone)
      , (10, RoleTokensUsePolicy <$> arbitrary)
      , (10, RoleTokensMint <$> arbitrary)
      , (1, RoleTokensUsePolicyWithOpenRoles <$> arbitrary <*> arbitrary <*> arbitrary)
      ]
  shrink = genericShrink

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
      , pure RequiresSingleThreadToken
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
      [ ContractCreated ReferenceTxInsScriptsInlineDatumsInBabbageEra <$> arbitrary
      , ContractCreated ReferenceTxInsScriptsInlineDatumsInConwayEra <$> arbitrary
      ]
  shrink (ContractCreated ReferenceTxInsScriptsInlineDatumsInBabbageEra created) =
    ContractCreated ReferenceTxInsScriptsInlineDatumsInBabbageEra <$> shrink created
  shrink (ContractCreated ReferenceTxInsScriptsInlineDatumsInConwayEra created) =
    ContractCreated ReferenceTxInsScriptsInlineDatumsInConwayEra <$> shrink created

instance (ArbitraryMarloweVersion v, IsCardanoEra era) => Arbitrary (ContractCreatedInEra era v) where
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
      <*> hedgehog (genTxBody cardanoEra)
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
      [ InputsApplied ReferenceTxInsScriptsInlineDatumsInBabbageEra <$> arbitrary
      , InputsApplied ReferenceTxInsScriptsInlineDatumsInConwayEra <$> arbitrary
      ]
  shrink (InputsApplied ReferenceTxInsScriptsInlineDatumsInBabbageEra created) =
    InputsApplied ReferenceTxInsScriptsInlineDatumsInBabbageEra <$> shrink created
  shrink (InputsApplied ReferenceTxInsScriptsInlineDatumsInConwayEra created) =
    InputsApplied ReferenceTxInsScriptsInlineDatumsInConwayEra <$> shrink created

instance (ArbitraryMarloweVersion v, IsCardanoEra era) => Arbitrary (InputsAppliedInEra era v) where
  arbitrary =
    InputsAppliedInEra Core.marloweVersion
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> hedgehog (genTxBody cardanoEra)
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
      [ WithdrawTx ReferenceTxInsScriptsInlineDatumsInBabbageEra <$> arbitrary
      , WithdrawTx ReferenceTxInsScriptsInlineDatumsInConwayEra <$> arbitrary
      ]
  shrink (WithdrawTx ReferenceTxInsScriptsInlineDatumsInBabbageEra created) =
    WithdrawTx ReferenceTxInsScriptsInlineDatumsInBabbageEra <$> shrink created
  shrink (WithdrawTx ReferenceTxInsScriptsInlineDatumsInConwayEra created) =
    WithdrawTx ReferenceTxInsScriptsInlineDatumsInConwayEra <$> shrink created

instance (ArbitraryMarloweVersion v, IsCardanoEra era) => Arbitrary (WithdrawTxInEra era v) where
  arbitrary =
    WithdrawTxInEra Core.marloweVersion
      <$> arbitrary
      <*> hedgehog (genTxBody cardanoEra)
  shrink WithdrawTxInEra{..} = [WithdrawTxInEra{..}{WithdrawTxInEra.inputs = inputs'} | inputs' <- shrink inputs]

instance ArbitraryCommand MarloweTxCommand where
  arbitraryTag =
    elements
      [ SomeTag $ TagCreate Core.MarloweV1
      , SomeTag $ TagApplyInputs Core.MarloweV1
      , SomeTag $ TagWithdraw Core.MarloweV1
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
    TagSubmit ->
      oneof
        [ Submit ReferenceTxInsScriptsInlineDatumsInBabbageEra <$> hedgehog (genTx BabbageEra)
        , Submit ReferenceTxInsScriptsInlineDatumsInConwayEra <$> hedgehog (genTx ConwayEra)
        ]
  arbitraryJobId = \case
    TagCreate Core.MarloweV1 -> Nothing
    TagApplyInputs Core.MarloweV1 -> Nothing
    TagWithdraw Core.MarloweV1 -> Nothing
    TagSubmit -> Just $ JobIdSubmit <$> arbitrary
  arbitraryStatus = \case
    TagCreate Core.MarloweV1 -> Nothing
    TagApplyInputs Core.MarloweV1 -> Nothing
    TagWithdraw Core.MarloweV1 -> Nothing
    TagSubmit -> Just arbitrary
  arbitraryErr = \case
    TagCreate Core.MarloweV1 -> Just arbitrary
    TagApplyInputs Core.MarloweV1 -> Just arbitrary
    TagWithdraw Core.MarloweV1 -> Just arbitrary
    TagSubmit -> Just arbitrary
  arbitraryResult = \case
    TagCreate Core.MarloweV1 -> arbitrary
    TagApplyInputs Core.MarloweV1 -> arbitrary
    TagWithdraw Core.MarloweV1 -> arbitrary
    TagSubmit -> arbitrary
  shrinkCommand = \case
    Create staking Core.MarloweV1 wallet roleConfig meta minAda contract ->
      concat
        [ Create
            <$> shrink staking
            <*> pure Core.MarloweV1
            <*> pure wallet
            <*> pure roleConfig
            <*> pure meta
            <*> pure minAda
            <*> pure contract
        , Create staking Core.MarloweV1
            <$> shrink wallet
            <*> pure roleConfig
            <*> pure meta
            <*> pure minAda
            <*> pure contract
        , Create staking Core.MarloweV1 wallet
            <$> shrink roleConfig
            <*> pure meta
            <*> pure minAda
            <*> pure contract
        , Create staking Core.MarloweV1 wallet roleConfig
            <$> shrink meta
            <*> pure minAda
            <*> pure contract
        , Create staking Core.MarloweV1 wallet roleConfig meta
            <$> shrink minAda
            <*> pure contract
        , Create staking Core.MarloweV1 wallet roleConfig meta minAda
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
    Submit _ _ -> []
  shrinkJobId = \case
    JobIdSubmit txId -> JobIdSubmit <$> shrink txId
  shrinkErr = \case
    TagCreate Core.MarloweV1 -> shrink
    TagApplyInputs Core.MarloweV1 -> shrink
    TagWithdraw Core.MarloweV1 -> shrink
    TagSubmit -> shrink
  shrinkResult = \case
    TagCreate Core.MarloweV1 -> shrink
    TagApplyInputs Core.MarloweV1 -> shrink
    TagWithdraw Core.MarloweV1 -> shrink
    TagSubmit -> shrink
  shrinkStatus = \case
    TagCreate Core.MarloweV1 -> \case {}
    TagApplyInputs Core.MarloweV1 -> \case {}
    TagWithdraw Core.MarloweV1 -> \case {}
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
    TagSubmit ->
      sconcat $
        NE.fromList
          [ Submit ReferenceTxInsScriptsInlineDatumsInBabbageEra <$> variations
          , Submit ReferenceTxInsScriptsInlineDatumsInConwayEra <$> variations
          ]
  jobIdVariations = \case
    TagCreate Core.MarloweV1 -> []
    TagApplyInputs Core.MarloweV1 -> []
    TagWithdraw Core.MarloweV1 -> []
    TagSubmit -> NE.toList $ JobIdSubmit <$> variations
  statusVariations = \case
    TagCreate Core.MarloweV1 -> []
    TagApplyInputs Core.MarloweV1 -> []
    TagWithdraw Core.MarloweV1 -> []
    TagSubmit -> NE.toList variations
  errVariations = \case
    TagCreate Core.MarloweV1 -> NE.toList variations
    TagApplyInputs Core.MarloweV1 -> NE.toList variations
    TagWithdraw Core.MarloweV1 -> NE.toList variations
    TagSubmit -> NE.toList variations
  resultVariations = \case
    TagCreate Core.MarloweV1 -> variations
    TagApplyInputs Core.MarloweV1 -> variations
    TagWithdraw Core.MarloweV1 -> variations
    TagSubmit -> variations
