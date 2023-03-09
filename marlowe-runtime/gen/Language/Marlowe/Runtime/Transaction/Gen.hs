{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE DataKinds #-}

module Language.Marlowe.Runtime.Transaction.Gen
  where

import Cardano.Api (IsCardanoEra, cardanoEra)
import Data.Foldable (fold)
import Gen.Cardano.Api.Typed (genTxBody)
import Language.Marlowe.Runtime.ChainSync.Gen ()
import qualified Language.Marlowe.Runtime.Core.Api as Core
import Language.Marlowe.Runtime.Core.Gen (ArbitraryMarloweVersion)
import Language.Marlowe.Runtime.History.Gen ()
import Language.Marlowe.Runtime.Transaction.Api
import qualified Language.Marlowe.Runtime.Transaction.Api as ContractCreated (ContractCreated(..))
import qualified Language.Marlowe.Runtime.Transaction.Api as InputsApplied (InputsApplied(..))
import Spec.Marlowe.Semantics.Arbitrary ()
import Test.QuickCheck hiding (shrinkMap)
import Test.QuickCheck.Hedgehog (hedgehog)
import Test.QuickCheck.Instances ()

instance Arbitrary WalletAddresses where
  arbitrary = WalletAddresses <$> arbitrary <*> arbitrary <*> arbitrary
  shrink = genericShrink

instance Arbitrary NFTMetadata where
  arbitrary = do
    mMetadata <- mkNFTMetadata <$> arbitrary
    maybe arbitrary pure mMetadata
  shrink = genericShrink

instance Arbitrary Mint where
  arbitrary = mkMint <$> arbitrary
  shrink = genericShrink

instance Arbitrary RoleTokensConfig where
  arbitrary = oneof
    [ pure RoleTokensNone
    , RoleTokensUsePolicy <$> arbitrary
    , RoleTokensMint <$> arbitrary
    ]
  shrink = genericShrink

instance Arbitrary SubmitStatus where
  arbitrary = elements [Submitting, Accepted ]
  shrink = genericShrink

instance Arbitrary LoadMarloweContextError where
  arbitrary = oneof
    [ pure LoadMarloweContextErrorNotFound
    , LoadMarloweContextErrorVersionMismatch <$> arbitrary
    , pure LoadMarloweContextToCardanoError
    , MarloweScriptNotPublished <$> arbitrary
    , PayoutScriptNotPublished <$> arbitrary
    , ExtractCreationError <$> arbitrary
    , ExtractMarloweTransactionError <$> arbitrary
    ]
  shrink = genericShrink

instance ArbitraryMarloweVersion v => Arbitrary (ConstraintError v) where
  arbitrary = oneof
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
  arbitrary = oneof
    [ pure MintingUtxoSelectionFailed
    , AddressDecodingFailed <$> arbitrary
    , MintingScriptDecodingFailed <$> arbitrary
    ]
  shrink = genericShrink

instance ArbitraryMarloweVersion v => Arbitrary (CreateError v) where
  arbitrary = oneof
    [ CreateConstraintError <$> arbitrary
    , CreateLoadMarloweContextFailed <$> arbitrary
    , CreateBuildupFailed <$> arbitrary
    , pure CreateToCardanoError
    ]
  shrink = genericShrink

instance Arbitrary ApplyInputsConstraintsBuildupError where
  arbitrary = oneof
    [ MarloweComputeTransactionFailed <$> arbitrary
    , pure UnableToDetermineTransactionTimeout
    ]
  shrink = genericShrink

instance ArbitraryMarloweVersion v => Arbitrary (ApplyInputsError v) where
  arbitrary = oneof
    [ ApplyInputsConstraintError <$> arbitrary
    , pure ScriptOutputNotFound
    , ApplyInputsLoadMarloweContextFailed <$> arbitrary
    , ApplyInputsConstraintsBuildupFailed <$> arbitrary
    , SlotConversionFailed <$> arbitrary
    , pure TipAtGenesis
    , ValidityLowerBoundTooHigh <$> arbitrary <*> arbitrary
    ]
  shrink = \case
    ApplyInputsConstraintError err -> ApplyInputsConstraintError <$> shrink err
    ScriptOutputNotFound -> []
    ApplyInputsLoadMarloweContextFailed err -> ApplyInputsLoadMarloweContextFailed <$> shrink err
    ApplyInputsConstraintsBuildupFailed err -> ApplyInputsConstraintsBuildupFailed <$> shrink err
    SlotConversionFailed err -> SlotConversionFailed <$> shrink err
    TipAtGenesis -> []
    ValidityLowerBoundTooHigh _ _ -> []

instance ArbitraryMarloweVersion v => Arbitrary (WithdrawError v) where
  arbitrary = oneof
    [ WithdrawConstraintError <$> arbitrary
    , WithdrawLoadMarloweContextFailed <$> arbitrary
    , UnableToFindPayoutForAGivenRole <$> arbitrary
    ]
  shrink = genericShrink

instance Arbitrary SubmitError where
  arbitrary = oneof
    [ SubmitException <$> arbitrary
    , SubmitFailed <$> arbitrary
    , pure TxDiscarded
    ]
  shrink = genericShrink

instance (ArbitraryMarloweVersion v, IsCardanoEra era) => Arbitrary (ContractCreated era v) where
  arbitrary = ContractCreated
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
  shrink ContractCreated{..} = fold
    [ [ ContractCreated{..} { ContractCreated.metadata = metadata' } | metadata' <- shrink metadata ]
    , [ ContractCreated{..} { ContractCreated.datum = datum' } | datum' <- shrink datum ]
    , [ ContractCreated{..} { ContractCreated.assets = assets' } | assets' <- shrink assets ]
    ]

instance (ArbitraryMarloweVersion v, IsCardanoEra era) => Arbitrary (InputsApplied era v) where
  arbitrary = InputsApplied Core.marloweVersion
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> hedgehog (genTxBody cardanoEra)
  shrink InputsApplied{..} = fold
    [ [ InputsApplied{..} { InputsApplied.metadata = metadata' } | metadata' <- shrink metadata ]
    , [ InputsApplied{..} { InputsApplied.input = input' } | input' <- shrink input ]
    , [ InputsApplied{..} { InputsApplied.output = output' } | output' <- shrink output ]
    , [ InputsApplied{..} { InputsApplied.inputs = inputs' } | inputs' <- shrink inputs ]
    ]
