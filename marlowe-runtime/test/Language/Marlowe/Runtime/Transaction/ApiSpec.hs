{-# LANGUAGE DataKinds #-}

module Language.Marlowe.Runtime.Transaction.ApiSpec
  ( spec
  ) where

import qualified Cardano.Api as C
import Data.Binary.Get (runGet)
import Data.Binary.Put (runPut)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Void (Void)
import Language.Marlowe.Runtime.ChainSync.Api (Address(Address), TxId(TxId), TxIx(TxIx), TxOutRef(TxOutRef))
import Language.Marlowe.Runtime.Core.Api (ContractId(ContractId), MarloweVersion(MarloweV1), MarloweVersionTag(V1))
import Language.Marlowe.Runtime.Transaction.Api
  (ApplyInputsError, MarloweTxCommand(ApplyInputs), Tag(TagApplyInputs), WalletAddresses(WalletAddresses))
import Language.Marlowe.Runtime.Transaction.Arbitrary (genByteString, genSet)
import Network.Protocol.Job.Types (Command(tagFromCommand), getCommand, getResult, putCommand, putResult)
import Spec.Marlowe.Semantics.Arbitrary (arbitraryPositiveInteger)
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Arbitrary(arbitrary), Gen)

genAddress :: Gen Address
genAddress = Address <$> genByteString

genTxOutRef :: Gen TxOutRef
genTxOutRef = TxOutRef <$> (TxId <$> genByteString) <*> (TxIx <$> arbitrary)

genContractId :: Gen ContractId
genContractId = ContractId <$> genTxOutRef

genWalletAddresses :: Gen WalletAddresses
genWalletAddresses = do
  changeAddress <- genAddress
  extraAddresses <- genSet genAddress
  collateralUtxos <- genSet genTxOutRef
  pure $ WalletAddresses changeAddress extraAddresses collateralUtxos

genApplyInputCmd :: Gen
  (MarloweTxCommand
     Void
     (ApplyInputsError 'V1)
     (C.TxBody C.BabbageEra))
genApplyInputCmd = do
  walletAddresses <- genWalletAddresses
  contractId <- genContractId
  redeemer <- arbitrary
  let
    genValidityBound = do
      arbitrary >>= \case
        True -> do
          posix <- fromInteger <$> arbitraryPositiveInteger
          pure $ Just (posixSecondsToUTCTime $ posix / 1000)
        False -> pure Nothing

  invalidBefore <- genValidityBound
  invalidHereafter <- genValidityBound

  pure $ ApplyInputs
    MarloweV1
    walletAddresses
    contractId
    invalidBefore
    invalidHereafter
    redeemer

spec :: Spec
spec = describe "MarloweTxCommand serialization" do
  prop "ApplyInputs command" do
    cmd <- genApplyInputCmd
    let
      encoded = runPut (putCommand cmd)
      getCmd = getCommand (tagFromCommand cmd)
      decoded = runGet getCmd encoded
    pure $ cmd `shouldBe` decoded

  it "handles ApplyInputs result" do
    let
      txIn = C.TxIn "059b32a85f41480ba6b83e210a8999f38d691ef1500286b9d67a769b74597a31" (C.TxIx 0)
      possibleTxBody =
        C.makeTransactionBody $ C.TxBodyContent
          { txIns = [(txIn, C.BuildTxWith $ C.KeyWitness C.KeyWitnessForSpending)]
          , txInsCollateral = C.TxInsCollateralNone
          , txInsReference = C.TxInsReferenceNone
          , txOuts = []
          , txTotalCollateral = C.TxTotalCollateralNone
          , txReturnCollateral = C.TxReturnCollateralNone
          , txFee = C.TxFeeExplicit C.TxFeesExplicitInBabbageEra 0
          , txValidityRange =
            ( C.TxValidityNoLowerBound
            , C.TxValidityNoUpperBound C.ValidityNoUpperBoundInBabbageEra
            )
          , txMetadata = C.TxMetadataNone
          , txAuxScripts = C.TxAuxScriptsNone
          , txExtraKeyWits = C.TxExtraKeyWitnessesNone
          , txProtocolParams = C.BuildTxWith Nothing
          , txWithdrawals = C.TxWithdrawalsNone
          , txCertificates = C.TxCertificatesNone
          , txUpdateProposal = C.TxUpdateProposalNone
          , txMintValue = C.TxMintNone
          , txScriptValidity = C.TxScriptValidity
              C.TxScriptValiditySupportedInBabbageEra
              C.ScriptValid
          }

    case possibleTxBody of
      Right txBody -> do
        let
          tag = TagApplyInputs MarloweV1
          encoded = runPut (putResult tag txBody)
          getCmdResult = getResult tag
          decoded = runGet getCmdResult encoded
        txBody `shouldBe` decoded
      Left err -> do
        fail $ "Fixture `TxBody` value creation failed: " <> show err


