-----------------------------------------------------------------------------
--
-- Module      :  $Headers
-- License     :  Apache 2.0
--
-- Stability   :  Experimental
-- Portability :  Portable
--
-- | Generate random data for Plutus tests.
--
-----------------------------------------------------------------------------


{-# LANGUAGE RecordWildCards #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}


module Spec.Marlowe.Plutus.Arbitrary (
  arbitrarySemanticsTransaction
, arbitraryPayoutTransaction
) where


import Data.Bifunctor (bimap, second)
import Data.List (nub, permutations)
import Data.Maybe (catMaybes)
import Language.Marlowe.Core.V1.Semantics
import Language.Marlowe.Core.V1.Semantics.Types hiding (Value)
import Language.Marlowe.Scripts
import Plutus.V1.Ledger.Api (Address (..), BuiltinData (..), Credential (..), Data (..), Datum (..), DatumHash (..),
                             Extended (..), Interval (..), LowerBound (..), PubKeyHash (..), ScriptContext (..),
                             ScriptPurpose (..), StakingCredential (..), TokenName, TxId (..), TxInInfo (..),
                             TxInfo (..), TxOut (..), TxOutRef (..), UpperBound (..), ValidatorHash (..), Value (..),
                             toBuiltin, toBuiltinData)
import Plutus.V1.Ledger.Value (gt)
import PlutusTx.Builtins (BuiltinByteString)
import Spec.Marlowe.Plutus.Script
import Spec.Marlowe.Semantics.Arbitrary
import Test.Tasty.QuickCheck (Arbitrary (..), Gen, elements, frequency, listOf, suchThat, vectorOf)

import qualified Data.ByteString as BS (ByteString, pack)
import qualified Data.ByteString.Char8 as BS8 (pack)
import qualified Plutus.V1.Ledger.Value as V (adaSymbol, adaToken, singleton)
import qualified PlutusTx.AssocMap as AM


instance Arbitrary Address where
  arbitrary = Address <$> arbitrary <*> arbitrary


instance Arbitrary BS.ByteString where
  arbitrary = BS8.pack <$> arbitrary


instance Arbitrary BuiltinByteString where
  arbitrary = toBuiltin . BS.pack <$> arbitrary


instance Arbitrary BuiltinData where
  arbitrary = BuiltinData <$> arbitrary


instance Arbitrary Credential where
  arbitrary =
    do
      isPubKey <- frequency [(9, pure True), (1, pure False)]
      if isPubKey
        then PubKeyCredential . PubKeyHash    <$> arbitraryByteString 28
        else ScriptCredential . ValidatorHash <$> arbitraryByteString 28


instance Arbitrary Data where
  arbitrary =
    let
      arbitraryDepth 0 =
        frequency
          [
            (1, I <$> arbitrary)
          , (2, B <$> arbitrary)
          ]
      arbitraryDepth n =
        frequency
          [
            ( 1, Constr <$> arbitrary <*> listOf (arbitraryDepth (n-1)) `suchThat` ((< 5) . length))
          , ( 2, Map    <$> listOf ((,) <$> arbitraryDepth (n-1) <*> arbitraryDepth (n-1)) `suchThat` ((< 5) . length))
          , ( 5, List   <$> listOf (arbitraryDepth (n-1)) `suchThat` ((< 5) . length))
          , (10, I      <$> arbitrary)
          , (20, B      <$> arbitrary)
          ]
    in
      arbitraryDepth (4 :: Int)


instance Arbitrary Datum where
  arbitrary = Datum <$> arbitrary


instance Arbitrary DatumHash where
  arbitrary = DatumHash <$> arbitrary


instance Arbitrary a => Arbitrary (Extended a) where
  arbitrary =
    frequency
      [
        (1, pure NegInf         )
      , (9, Finite <$> arbitrary)
      , (1, pure PosInf         )
      ]


instance Arbitrary a => Arbitrary (Interval a) where
  arbitrary = Interval <$> arbitrary <*> arbitrary


instance Arbitrary a => Arbitrary (LowerBound a) where
  arbitrary = LowerBound <$> arbitrary <*> arbitrary


instance Arbitrary ScriptContext where
  arbitrary = ScriptContext <$> arbitrary <*> (Spending <$> arbitrary)


instance Arbitrary StakingCredential where
  arbitrary = StakingHash <$> arbitrary


instance Arbitrary TxId where
  arbitrary = TxId <$> arbitraryByteString 32


instance Arbitrary TxInfo where
  arbitrary =
    do
      txInfoInputs <- arbitrary
      txInfoOutputs <- arbitrary
      txInfoFee <- V.singleton V.adaSymbol V.adaToken <$> arbitraryPositiveInteger
      txInfoValidRange <- arbitrary
      txInfoSignatories <- arbitrary
      txInfoData <- arbitrary
      let
        txInfoMint = mempty
        txInfoDCert = mempty
        txInfoWdrl = mempty
      txInfoId <- arbitrary
      pure TxInfo{..}


instance Arbitrary TxInInfo where
  arbitrary = TxInInfo <$> arbitrary <*> arbitrary


instance Arbitrary TxOut where
  arbitrary = TxOut <$> arbitrary <*> arbitrary `suchThat` (`gt` mempty) <*> arbitrary


instance Arbitrary TxOutRef where
  arbitrary = TxOutRef <$> arbitrary <*> arbitrary `suchThat` (> 0)


instance Arbitrary a => Arbitrary (UpperBound a) where
  arbitrary = UpperBound <$> arbitrary <*> arbitrary


instance Arbitrary ValidatorHash where
  arbitrary = ValidatorHash <$> arbitrary


instance Arbitrary Value where
  arbitrary = Value <$> arbitraryAssocMap arbitrary (arbitraryAssocMap arbitrary arbitrary)


instance Arbitrary MarloweData where
  arbitrary = MarloweData <$> arbitrary <*> arbitrary


instance Arbitrary MarloweTxInput where
  arbitrary = Input <$> arbitrary  -- FIXME: Add merkleized case.


-- | Generate an arbitrary bytestring of specified length.
arbitraryByteString :: Int -> Gen BuiltinByteString
arbitraryByteString n = toBuiltin . BS.pack <$> vectorOf n arbitrary


-- | Generate an arbitrary, valid Marlowe semantics transaction: datum, redeemer, and script context.
arbitrarySemanticsTransaction :: Bool -> Gen (MarloweParams, MarloweData, MarloweInput, ScriptContext, TransactionOutput)
arbitrarySemanticsTransaction noisy =
  do
    (marloweState, marloweContract, TransactionInput{..}, output) <- arbitraryGoldenTransaction
    rolesCurrency <- arbitrary
    let
      marloweData = MarloweData{..}
      rolePayoutValidatorHash = mkRolePayoutValidatorHash rolesCurrency
      marloweParams = MarloweParams{..}
      inputToMarloweTxInput (NormalInput     content              ) = (Input             content     , Nothing              )
      inputToMarloweTxInput (MerkleizedInput content hash contract) = (MerkleizedTxInput content hash, Just (hash, contract))
      (marloweInput, merkleizations) = second catMaybes . unzip $ inputToMarloweTxInput <$> txInputs
      ownAddress = semanticsAddress marloweParams
      payAddress = payoutAddress marloweParams
      totalValue = foldMap (\((_, Token c n), i) -> V.singleton c n i) . AM.toList . accounts
    (scriptInput@TxInInfo{txInInfoOutRef=scriptInputRef}, inDatumPair) <-
      do
        txInInfoOutRef <- arbitrary
        let
          inDatum = Datum $ toBuiltinData marloweData
          inDatumHash = hashMarloweData marloweData
          txInInfoResolved = TxOut ownAddress (totalValue marloweState) (Just inDatumHash)
        pure (TxInInfo{..}, (inDatumHash, inDatum))
    deposits <-
      fmap concat
        $ sequence
        [
          do
            ref <- arbitrary
            address  <- arbitrary
            case input of
              IDeposit _ (PK   pkh) (Token c n) i -> pure [TxInInfo ref $ TxOut (Address (PubKeyCredential pkh) Nothing) (V.singleton c n i) Nothing]
              IDeposit _ (Role _  ) (Token c n) i -> pure [TxInInfo ref $ TxOut address                                  (V.singleton c n i) Nothing]
              _                                   -> pure []
        |
          input <- getInputContent <$> txInputs
        ]
    rolesIn <-
      fmap concat
        $ sequence
        [
          do
            ref <- arbitrary
            address  <- arbitrary
            case input of
              IDeposit _ (Role role) _ _         -> pure [TxInInfo ref $ TxOut address (V.singleton rolesCurrency role 1) Nothing]
              IChoice (ChoiceId _ (Role role)) _ -> pure [TxInInfo ref $ TxOut address (V.singleton rolesCurrency role 1) Nothing]
              _                                  -> pure []
        |
          input <- getInputContent <$> txInputs
        ]
    noisyInputs <- if noisy then arbitrary `suchThat` ((< 5) . length) else pure []
    txInfoInputs <- elements . permutations $ [scriptInput] <> deposits <> rolesIn <> noisyInputs
    let
      (outState, outContract, outPayments) =
        case output of
          TransactionOutput{..} -> (txOutState, txOutContract, txOutPayments)
          _                     -> error "Not a golden transaction."
      (scriptOutput, outDatumPair) =
        unzip
          [
            let
              outData = MarloweData outState outContract
              outDatum = Datum $ toBuiltinData outData
              outDatumHash = hashMarloweData outData
            in
              (
                TxOut ownAddress (totalValue outState) (Just outDatumHash)
              , (outDatumHash, outDatum)
              )
          |
            outContract /= Close
          ]
      (paymentOutput, paymentHashes) =
        second catMaybes
          . unzip
          $ concat
          [
            case party of
              Party (PK   pkh ) -> [(TxOut (Address (PubKeyCredential pkh) Nothing) value Nothing, Nothing)]
              Party (Role role) -> [(TxOut payAddress value (Just $ hashRole role), Just (hashRole role, Datum $ toBuiltinData role))]
              _                 -> []
          |
            Payment _ party value <- outPayments
          ]
    rolesOut <-
      sequence
        [
          TxOut <$> arbitrary <*> pure token <*> pure Nothing
        |
          TxInInfo _ (TxOut _ token _) <- rolesIn
        ]
    noisyOutputs <- if noisy then arbitrary `suchThat` ((< 5) . length) else pure []
    txInfoOutputs <- elements . permutations $ scriptOutput <> paymentOutput <> rolesOut <> noisyOutputs
    let
      txInfoValidRange =
        Interval
          (LowerBound (Finite $ fst txInterval) True)
          (UpperBound (Finite $ snd txInterval) True)
    txInfoSignatories <-
      elements . permutations . nub . concat
        $  [
             case input of
               IDeposit _ (PK    pkh) _ _      -> [pkh]
               IChoice (ChoiceId _ (PK pkh)) _ -> [pkh]
               _                               -> []
           |
             input <- getInputContent <$> txInputs
           ]
        <> [
             case credential of
               PubKeyCredential pkh -> [pkh]
               _                    -> []
           |
             TxInInfo _ (TxOut (Address credential _) _ _ ) <- txInfoInputs
           ]
    noisyData <- if noisy then arbitrary `suchThat` ((< 5) . length) else pure []
    txInfoData <-
      elements . permutations
        $  [inDatumPair]
        <> (bimap DatumHash (Datum . toBuiltinData) <$> merkleizations)
        <> outDatumPair
        <> paymentHashes
        <> noisyData
    let
      txInfoWdrl = mempty
      txInfoMint = mempty
      txInfoDCert = mempty
    txInfoId <- arbitrary
    txInfoFee <- V.singleton V.adaSymbol V.adaToken <$> arbitraryPositiveInteger
    let
      scriptContextTxInfo = TxInfo{..}
      scriptContextPurpose = Spending scriptInputRef
    pure (marloweParams, marloweData, marloweInput, ScriptContext{..}, output)


-- | Generate an arbitrary, valid Marlowe payout transaction: datum, redeemer, and script context.
arbitraryPayoutTransaction :: Bool -> Gen (MarloweParams, TokenName, ScriptContext)
arbitraryPayoutTransaction noisy =
  do
    rolesCurrency <- arbitrary
    role <- arbitrary
    value <- arbitrary `suchThat` (`gt` mempty)
    let
      rolePayoutValidatorHash = mkRolePayoutValidatorHash rolesCurrency
      marloweParams = MarloweParams{..}
      ownAddress = payoutAddress marloweParams
    (scriptInput@TxInInfo{txInInfoOutRef=scriptInputRef}, inDatumPair) <-
      do
        txInInfoOutRef <- arbitrary
        let
          inDatum = Datum $ toBuiltinData role
          inDatumHash = hashRole role
          txInInfoResolved = TxOut ownAddress value (Just inDatumHash)
        pure (TxInInfo{..}, (inDatumHash, inDatum))
    rolesIn <-
      do
        ref <- arbitrary
        address  <- arbitrary
        pure [TxInInfo ref $ TxOut address (V.singleton rolesCurrency role 1) Nothing]
    noisyInputs <- if noisy then arbitrary `suchThat` ((< 5) . length) else pure []
    txInfoInputs <- elements . permutations $ [scriptInput] <> rolesIn <> noisyInputs
    noisyOutputs <- if noisy then arbitrary `suchThat` ((< 5) . length) else pure []
    txInfoOutputs <- elements . permutations $ noisyOutputs
    let
      txInfoValidRange =
        Interval
          (LowerBound NegInf False)
          (UpperBound PosInf False)
    txInfoSignatories <-
      elements . permutations . nub . concat
        $  [
             case credential of
               PubKeyCredential pkh -> [pkh]
               _                    -> []
           |
             TxInInfo _ (TxOut (Address credential _) _ _ ) <- txInfoInputs
           ]
    noisyData <- if noisy then arbitrary `suchThat` ((< 5) . length) else pure []
    txInfoData <-
      elements . permutations
        $  [inDatumPair]
        <> noisyData
    let
      txInfoWdrl = mempty
      txInfoMint = mempty
      txInfoDCert = mempty
    txInfoId <- arbitrary
    txInfoFee <- V.singleton V.adaSymbol V.adaToken <$> arbitraryPositiveInteger
    let
      scriptContextTxInfo = TxInfo{..}
      scriptContextPurpose = Spending scriptInputRef
    pure (marloweParams, role, ScriptContext{..})
