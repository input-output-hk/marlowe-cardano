{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}

module Language.Marlowe.Runtime.Transaction.CommandSpec
  where

import Cardano.Api (BabbageEra, CardanoEra(..))
import Data.Foldable (fold)
import qualified Data.List.NonEmpty as NE
import Data.Void (absurd)
import GHC.Show (showSpace)
import Gen.Cardano.Api.Typed (genTx, genTxBody)
import qualified Language.Marlowe.Core.V1.Semantics as V1
import Language.Marlowe.Protocol.Common ()
import Language.Marlowe.Protocol.SyncSpec (genTransactionScriptOutput, shrinkTransactionScriptOutput)
import Language.Marlowe.Runtime.ChainSync.Gen ()
import Language.Marlowe.Runtime.Core.Api
import Language.Marlowe.Runtime.History.Api
import qualified Language.Marlowe.Runtime.History.Api as History
import Language.Marlowe.Runtime.Plutus.V2.Api (toPlutusCurrencySymbol)
import Language.Marlowe.Runtime.Transaction.Api
import qualified Language.Marlowe.Runtime.Transaction.Api as ContractCreated (ContractCreated(..))
import qualified Language.Marlowe.Runtime.Transaction.Api as InputsApplied (InputsApplied(..))
import qualified Language.Marlowe.Runtime.Transaction.Api as Tx
import Network.Protocol.Codec.Spec (checkPropCodec, genByteStringSplits)
import Network.Protocol.Job.Codec (codecJob)
import Network.Protocol.Job.Types
import Spec.Marlowe.Semantics.Arbitrary ()
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck
import Test.QuickCheck.Hedgehog (hedgehog)
import Test.QuickCheck.Instances ()

spec :: Spec
spec = describe "MarloweTxCommand" do
  prop "It has a lawful Job protocol codec" $ checkPropCodec genByteStringSplits $ codecJob @MarloweTxCommand

instance Arbitrary WalletAddresses where
  arbitrary = WalletAddresses <$> arbitrary <*> arbitrary <*> arbitrary
  shrink = genericShrink

instance Arbitrary NFTMetadata where
  arbitrary = do
    result <- mkNFTMetadata <$> arbitrary
    maybe arbitrary pure result
  shrink = genericShrink

instance Arbitrary Mint where
  arbitrary = mkMint . NE.fromList <$> listOf1 arbitrary
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

instance Arbitrary ExtractCreationError where
  arbitrary = elements
    [ TxIxNotFound
    , ByronAddress
    , NonScriptAddress
    , InvalidScriptHash
    , NoCreateDatum
    , InvalidCreateDatum
    , NotCreationTransaction
    ]
  shrink = genericShrink

instance Arbitrary ExtractMarloweTransactionError where
  arbitrary = oneof
    [ pure TxInNotFound
    , pure NoRedeemer
    , pure InvalidRedeemer
    , pure NoTransactionDatum
    , pure InvalidTransactionDatum
    , NoPayoutDatum <$> arbitrary
    , InvalidPayoutDatum <$> arbitrary
    , pure InvalidValidityRange
    , pure History.SlotConversionFailed
    ]
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
    , HandshakeFailed <$> arbitrary
    ]
  shrink = genericShrink

instance Arbitrary (ConstraintError 'V1) where
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
  shrink = genericShrink

instance Arbitrary CreateBuildupError where
  arbitrary = oneof
    [ pure MintingUtxoSelectionFailed
    , AddressDecodingFailed <$> arbitrary
    , MintingScriptDecodingFailed <$> arbitrary
    ]
  shrink = genericShrink

instance Arbitrary (CreateError 'V1) where
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

instance Arbitrary (ApplyInputsError 'V1) where
  arbitrary = oneof
    [ ApplyInputsConstraintError <$> arbitrary
    , pure ScriptOutputNotFound
    , ApplyInputsLoadMarloweContextFailed <$> arbitrary
    , ApplyInputsConstraintsBuildupFailed <$> arbitrary
    , Tx.SlotConversionFailed <$> arbitrary
    , pure TipAtGenesis
    , ValidityLowerBoundTooHigh <$> arbitrary <*> arbitrary
    ]
  shrink = genericShrink

instance Arbitrary (WithdrawError 'V1) where
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

instance Arbitrary (ContractCreated BabbageEra 'V1) where
  arbitrary = ContractCreated
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> pure MarloweV1
    <*> (V1.MarloweData <$> (V1.MarloweParams . toPlutusCurrencySymbol <$> arbitrary) <*> arbitrary <*> arbitrary)
    <*> arbitrary
    <*> hedgehog (genTxBody BabbageEra)
  shrink ContractCreated{..} = fold
    [ [ ContractCreated{..} { ContractCreated.metadata = metadata' } | metadata' <- shrink metadata ]
    , [ ContractCreated{..} { ContractCreated.datum = datum' } | datum' <- shrinkMarloweData datum ]
    , [ ContractCreated{..} { ContractCreated.assets = assets' } | assets' <- shrink assets ]
    ]

instance Arbitrary (InputsApplied BabbageEra 'V1) where
  arbitrary = InputsApplied MarloweV1
    <$> arbitrary
    <*> arbitrary
    <*> genTransactionScriptOutput MarloweV1
    <*> oneof [pure Nothing, Just <$> genTransactionScriptOutput MarloweV1]
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> hedgehog (genTxBody BabbageEra)
  shrink InputsApplied{..} = fold
    [ [ InputsApplied{..} { InputsApplied.metadata = metadata' } | metadata' <- shrink metadata ]
    , [ InputsApplied{..} { InputsApplied.input = input' } | input' <- shrinkTransactionScriptOutput MarloweV1 input ]
    , [ InputsApplied{..} { InputsApplied.output = output' } | output' <- shrinkMaybe (shrinkTransactionScriptOutput MarloweV1) output ]
    , [ InputsApplied{..} { InputsApplied.inputs = inputs' } | inputs' <- shrink inputs ]
    ]

shrinkMaybe :: (a -> [a]) -> Maybe a -> [Maybe a]
shrinkMaybe f = \case
  Nothing -> []
  Just a -> Nothing : (Just <$> f a)

shrinkMarloweData :: V1.MarloweData -> [V1.MarloweData]
shrinkMarloweData V1.MarloweData{..} = fold
  [ [ V1.MarloweData{..} { V1.marloweState = marloweState' } | marloweState' <- shrink marloweState ]
  , [ V1.MarloweData{..} { V1.marloweContract = marloweContract' } | marloweContract' <- shrink marloweContract ]
  ]

instance ArbitraryCommand MarloweTxCommand where
  arbitraryTag = elements
    [ SomeTag $ TagCreate MarloweV1
    , SomeTag $ TagApplyInputs MarloweV1
    , SomeTag $ TagWithdraw MarloweV1
    , SomeTag TagSubmit
    ]

  arbitraryCmd = \case
    TagCreate MarloweV1 -> Create
      <$> arbitrary
      <*> pure MarloweV1
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
    TagApplyInputs MarloweV1 -> ApplyInputs MarloweV1
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
    TagWithdraw MarloweV1 -> Withdraw MarloweV1
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
    TagSubmit -> hedgehog $ Submit <$> genTx BabbageEra

  arbitraryJobId = \case
    TagCreate MarloweV1 -> Nothing
    TagApplyInputs MarloweV1 -> Nothing
    TagWithdraw MarloweV1 -> Nothing
    TagSubmit -> Just $ JobIdSubmit <$> arbitrary

  arbitraryStatus = \case
    TagCreate MarloweV1 -> Nothing
    TagApplyInputs MarloweV1 -> Nothing
    TagWithdraw MarloweV1 -> Nothing
    TagSubmit -> Just arbitrary

  arbitraryErr = \case
    TagCreate MarloweV1 -> Just arbitrary
    TagApplyInputs MarloweV1 -> Just arbitrary
    TagWithdraw MarloweV1 -> Just arbitrary
    TagSubmit -> Just arbitrary

  arbitraryResult = \case
    TagCreate MarloweV1 -> arbitrary
    TagApplyInputs MarloweV1 -> arbitrary
    TagWithdraw MarloweV1 -> hedgehog $ genTxBody BabbageEra
    TagSubmit -> arbitrary

  shrinkCommand = \case
    Create stake MarloweV1 wallet roleTokenConfig metadata minAda contract -> fold
      [ [ Create stake MarloweV1 wallet' roleTokenConfig metadata minAda contract | wallet' <- shrink wallet ]
      , [ Create stake MarloweV1 wallet roleTokenConfig' metadata minAda contract | roleTokenConfig' <- shrink roleTokenConfig ]
      , [ Create stake MarloweV1 wallet roleTokenConfig metadata' minAda contract | metadata' <- shrink metadata ]
      , [ Create stake MarloweV1 wallet roleTokenConfig metadata minAda contract' | contract' <- shrink contract ]
      ]
    ApplyInputs MarloweV1 wallet contractId metadata invalidBefore invalidHereafter inputs -> fold
      [ [ ApplyInputs MarloweV1 wallet' contractId metadata invalidBefore invalidHereafter inputs | wallet' <- shrink wallet ]
      , [ ApplyInputs MarloweV1 wallet contractId metadata' invalidBefore invalidHereafter inputs | metadata' <- shrink metadata ]
      , [ ApplyInputs MarloweV1 wallet contractId metadata invalidBefore invalidHereafter inputs' | inputs' <- shrink inputs ]
      ]
    Withdraw MarloweV1 wallet contractId role ->
      [ Withdraw MarloweV1 wallet' contractId role | wallet' <- shrink wallet ]
    Submit{}-> []

  shrinkJobId = \case
    JobIdSubmit{} -> []

  shrinkErr = \case
    TagCreate MarloweV1 -> shrink
    TagApplyInputs MarloweV1 -> shrink
    TagWithdraw MarloweV1 -> shrink
    TagSubmit -> shrink

  shrinkResult = \case
    TagCreate MarloweV1 -> shrink
    TagApplyInputs MarloweV1 -> shrink
    TagWithdraw MarloweV1 -> const []
    TagSubmit -> shrink

  shrinkStatus = \case
    TagCreate MarloweV1 -> absurd
    TagApplyInputs MarloweV1 -> absurd
    TagWithdraw MarloweV1 -> absurd
    TagSubmit -> shrink

instance CommandEq MarloweTxCommand where
  commandEq = \case
    Create stake MarloweV1 wallet roleTokenConfig metadata minAda contract -> \case
      Create stake' MarloweV1 wallet' roleTokenConfig' metadata' minAda' contract' ->
        stake == stake'
          && wallet == wallet'
          && roleTokenConfig == roleTokenConfig'
          && metadata == metadata'
          && minAda == minAda'
          && contract == contract'
    ApplyInputs MarloweV1 wallet contractId metadata invalidBefore invalidHereafter inputs -> \case
      ApplyInputs MarloweV1 wallet' contractId' metadata' invalidBefore' invalidHereafter' inputs' ->
        wallet == wallet'
          && contractId == contractId'
          && metadata == metadata'
          && invalidBefore == invalidBefore'
          && invalidHereafter == invalidHereafter'
          && inputs == inputs'
    Withdraw MarloweV1 wallet contractId role -> \case
      Withdraw MarloweV1 wallet' contractId' role' ->
        wallet == wallet'
          && contractId == contractId'
          && role == role'
    Submit tx -> \case
      Submit tx' -> tx == tx'

  jobIdEq = \case
    JobIdSubmit txId -> \case
      JobIdSubmit txId' -> txId == txId'

  statusEq = \case
    TagCreate MarloweV1 -> (==)
    TagApplyInputs MarloweV1 -> (==)
    TagWithdraw MarloweV1 -> (==)
    TagSubmit -> (==)

  errEq = \case
    TagCreate MarloweV1 -> (==)
    TagApplyInputs MarloweV1 -> (==)
    TagWithdraw MarloweV1 -> (==)
    TagSubmit -> (==)

  resultEq = \case
    TagCreate MarloweV1 -> (==)
    TagApplyInputs MarloweV1 -> (==)
    TagWithdraw MarloweV1 -> (==)
    TagSubmit -> (==)

instance ShowCommand MarloweTxCommand where
  showsPrecTag p = \case
    TagCreate MarloweV1 -> showParen (p >= 11)
      ( showString "TagCreate"
      . showSpace
      . showString "MarloweV1"
      )
    TagApplyInputs MarloweV1 -> showParen (p >= 11)
      ( showString "TagApplyInputs"
      . showSpace
      . showString "MarloweV1"
      )
    TagWithdraw MarloweV1 -> showParen (p >= 11)
      ( showString "TagWithdraw"
      . showSpace
      . showString "MarloweV1"
      )
    TagSubmit -> showString "TagSubmit"

  showsPrecCommand p = showParen (p >= 11) . \case
    Create stake MarloweV1 wallet roleTokenConfig metadata minAda contract ->
      ( showString "Create"
      . showSpace
      . showsPrec 11 stake
      . showSpace
      . showsPrec 11 MarloweV1
      . showSpace
      . showsPrec 11 wallet
      . showSpace
      . showsPrec 11 roleTokenConfig
      . showSpace
      . showsPrec 11 metadata
      . showSpace
      . showsPrec 11 minAda
      . showSpace
      . showsPrec 11 contract
      )
    ApplyInputs MarloweV1 wallet contractId metadata invalidBefore invalidHereafter inputs ->
      ( showString "ApplyInputs"
      . showSpace
      . showsPrec 11 MarloweV1
      . showSpace
      . showsPrec 11 wallet
      . showSpace
      . showsPrec 11 contractId
      . showSpace
      . showsPrec 11 metadata
      . showSpace
      . showsPrec 11 invalidBefore
      . showSpace
      . showsPrec 11 invalidHereafter
      . showSpace
      . showsPrec 11 inputs
      )
    Withdraw MarloweV1 wallet contractId role ->
      ( showString "Withdraw"
      . showSpace
      . showsPrec 11 MarloweV1
      . showSpace
      . showsPrec 11 wallet
      . showSpace
      . showsPrec 11 contractId
      . showSpace
      . showsPrec 11 role
      )
    Submit tx ->
      ( showString "Submit"
      . showSpace
      . showsPrec 11 tx
      )

  showsPrecJobId p = \case
    JobIdSubmit txId -> showParen (p >= 11)
      ( showString "JobIdSubmit"
      . showSpace
      . showsPrec 11 txId
      )

  showsPrecStatus p = \case
    TagCreate MarloweV1 -> showsPrec p
    TagApplyInputs MarloweV1 -> showsPrec p
    TagWithdraw MarloweV1 -> showsPrec p
    TagSubmit -> showsPrec p

  showsPrecErr p = \case
    TagCreate MarloweV1 -> showsPrec p
    TagApplyInputs MarloweV1 -> showsPrec p
    TagWithdraw MarloweV1 -> showsPrec p
    TagSubmit -> showsPrec p

  showsPrecResult p = \case
    TagCreate MarloweV1 -> showsPrec p
    TagApplyInputs MarloweV1 -> showsPrec p
    TagWithdraw MarloweV1 -> showsPrec p
    TagSubmit -> showsPrec p
