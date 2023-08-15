{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Language.Marlowe.Runtime.Transaction.Gen where

import Cardano.Api (IsCardanoEra, cardanoEra)
import Cardano.Api.Shelley (
  ReferenceTxInsScriptsInlineDatumsSupportedInEra (ReferenceTxInsScriptsInlineDatumsInBabbageEra),
 )
import Control.Applicative (liftA2)
import qualified Data.ByteString.Char8 as BS
import Data.Foldable (fold)
import Gen.Cardano.Api.Typed (genTxBody)
import Language.Marlowe.Runtime.ChainSync.Gen ()
import qualified Language.Marlowe.Runtime.Core.Api as Core
import Language.Marlowe.Runtime.Core.Gen (ArbitraryMarloweVersion)
import Language.Marlowe.Runtime.History.Gen ()
import Language.Marlowe.Runtime.Transaction.Api
import qualified Language.Marlowe.Runtime.Transaction.Api as ContractCreatedInEra (ContractCreatedInEra (..))
import qualified Language.Marlowe.Runtime.Transaction.Api as InputsAppliedInEra (InputsAppliedInEra (..))
import qualified Language.Marlowe.Runtime.Transaction.Api as WithdrawTxInEra (WithdrawTxInEra (..))
import Network.HTTP.Media (MediaType, (//))
import qualified Network.URI
import qualified Network.URI as Network
import Spec.Marlowe.Semantics.Arbitrary ()
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

instance Arbitrary Mint where
  arbitrary = mkMint <$> arbitrary
  shrink = genericShrink

instance Arbitrary RoleTokensConfig where
  arbitrary =
    oneof
      [ pure RoleTokensNone
      , RoleTokensUsePolicy <$> arbitrary
      , RoleTokensMint <$> arbitrary
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

instance (ArbitraryMarloweVersion v) => Arbitrary (ConstraintError v) where
  arbitrary =
    oneof
      [ MintingUtxoNotFound <$> arbitrary
      , RoleTokenNotFound <$> arbitrary
      , pure ToCardanoError
      , pure MissingMarloweInput
      , PayoutInputNotFound <$> arbitrary
      , CalculateMinUtxoFailed <$> arbitrary
      , CoinSelectionFailed <$> arbitrary
      , BalancingError <$> arbitrary
      ]
  shrink = \case
    MintingUtxoNotFound err -> MintingUtxoNotFound <$> shrink err
    RoleTokenNotFound _ -> []
    ToCardanoError -> []
    MissingMarloweInput -> []
    PayoutInputNotFound _ -> []
    CalculateMinUtxoFailed err -> CalculateMinUtxoFailed <$> shrink err
    CoinSelectionFailed err -> CoinSelectionFailed <$> shrink err
    BalancingError err -> BalancingError <$> shrink err

instance Arbitrary CreateBuildupError where
  arbitrary =
    oneof
      [ pure MintingUtxoSelectionFailed
      , AddressDecodingFailed <$> arbitrary
      , MintingScriptDecodingFailed <$> arbitrary
      ]
  shrink = genericShrink

instance (ArbitraryMarloweVersion v) => Arbitrary (CreateError v) where
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

instance (ArbitraryMarloweVersion v) => Arbitrary (ApplyInputsError v) where
  arbitrary =
    oneof
      [ ApplyInputsEraUnsupported <$> arbitrary
      , ApplyInputsConstraintError <$> arbitrary
      , pure ScriptOutputNotFound
      , ApplyInputsLoadMarloweContextFailed <$> arbitrary
      , ApplyInputsConstraintsBuildupFailed <$> arbitrary
      , SlotConversionFailed <$> arbitrary
      , pure TipAtGenesis
      , ValidityLowerBoundTooHigh <$> arbitrary <*> arbitrary
      ]
  shrink = genericShrink

instance (ArbitraryMarloweVersion v) => Arbitrary (WithdrawError v) where
  arbitrary =
    oneof
      [ WithdrawConstraintError <$> arbitrary
      , WithdrawEraUnsupported <$> arbitrary
      , WithdrawLoadMarloweContextFailed <$> arbitrary
      , UnableToFindPayoutForAGivenRole <$> arbitrary
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
  arbitrary = ContractCreated ReferenceTxInsScriptsInlineDatumsInBabbageEra <$> arbitrary
  shrink (ContractCreated ReferenceTxInsScriptsInlineDatumsInBabbageEra created) =
    ContractCreated ReferenceTxInsScriptsInlineDatumsInBabbageEra <$> shrink created

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
  arbitrary = InputsApplied ReferenceTxInsScriptsInlineDatumsInBabbageEra <$> arbitrary
  shrink (InputsApplied ReferenceTxInsScriptsInlineDatumsInBabbageEra created) =
    InputsApplied ReferenceTxInsScriptsInlineDatumsInBabbageEra <$> shrink created

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
  arbitrary = WithdrawTx ReferenceTxInsScriptsInlineDatumsInBabbageEra <$> arbitrary
  shrink (WithdrawTx ReferenceTxInsScriptsInlineDatumsInBabbageEra created) =
    WithdrawTx ReferenceTxInsScriptsInlineDatumsInBabbageEra <$> shrink created

instance (ArbitraryMarloweVersion v, IsCardanoEra era) => Arbitrary (WithdrawTxInEra era v) where
  arbitrary =
    WithdrawTxInEra Core.marloweVersion
      <$> arbitrary
      <*> arbitrary
      <*> hedgehog (genTxBody cardanoEra)
  shrink WithdrawTxInEra{..} = [WithdrawTxInEra{..}{WithdrawTxInEra.inputs = inputs'} | inputs' <- shrink inputs]
