{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Language.Marlowe.Runtime.ChainSync.Gen where

import Cardano.Api (
  AddressAny (..),
  AsType (..),
  CardanoEra (..),
  CardanoMode,
  ConsensusMode (..),
  EraHistory (..),
  Key (verificationKeyHash),
  NetworkId (..),
  NetworkMagic (..),
  PlutusScriptVersion (..),
  ScriptDataSupportedInEra (..),
  SerialiseAsRawBytes (..),
  SystemStart (..),
  hashScriptDataBytes,
  unsafeHashableScriptData,
 )
import Cardano.Api.Byron (AnyCardanoEra (..))
import qualified Cardano.Api.Shelley as Shelley
import Control.Monad (replicateM)
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.ByteString.Short (fromShort)
import qualified Data.List.NonEmpty as NE
import Data.Maybe (mapMaybe)
import Data.SOP.Counting (Exactly (..))
import Data.SOP.Strict (K (..), NP (..))
import qualified Data.Set.NonEmpty as NESet
import qualified Data.Text as T
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Void (absurd)
import Data.Word (Word64)
import Language.Marlowe.Runtime.Cardano.Api (fromCardanoDatumHash, toCardanoScriptData)
import qualified Language.Marlowe.Runtime.Cardano.Api as Cardano.Api
import Language.Marlowe.Runtime.ChainSync.Api
import qualified Network.Protocol.ChainSeek.Types as ChainSeek
import qualified Network.Protocol.Job.Types as Command
import qualified Network.Protocol.Query.Types as Query
import Ouroboros.Consensus.Block (EpochSize (..))
import Ouroboros.Consensus.BlockchainTime (RelativeTime (..), SlotLength (..), mkSlotLength)
import Ouroboros.Consensus.HardFork.History (
  Bound (..),
  EraEnd (..),
  EraParams (..),
  EraSummary (..),
  SafeZone (..),
  mkInterpreter,
  summaryWithExactly,
 )
import Test.Gen.Cardano.Api.Typed (
  genAddressShelley,
  genPlutusScript,
  genProtocolParameters,
  genScriptHash,
  genTx,
  genVerificationKey,
 )
import Test.QuickCheck hiding (shrinkMap)
import Test.QuickCheck.Gen (chooseWord64)
import Test.QuickCheck.Hedgehog (hedgehog)

instance Arbitrary NetworkId where
  arbitrary =
    oneof
      [ pure Mainnet
      , Testnet . NetworkMagic <$> arbitrary
      ]
  shrink Mainnet = []
  shrink (Testnet (NetworkMagic m)) = Mainnet : (Testnet . NetworkMagic <$> shrink m)

instance (Arbitrary a) => Arbitrary (WithGenesis a) where
  arbitrary = oneof [pure Genesis, At <$> arbitrary]
  shrink = genericShrink

instance Arbitrary BlockHeader where
  arbitrary = BlockHeader <$> arbitrary <*> arbitrary <*> arbitrary
  shrink = genericShrink

instance Arbitrary SlotNo where
  arbitrary = SlotNo <$> arbitrary
  shrink = genericShrink

instance Arbitrary BlockNo where
  arbitrary = BlockNo <$> arbitrary
  shrink = genericShrink

instance Arbitrary BlockHeaderHash where
  arbitrary = BlockHeaderHash <$> genNBytes 32

instance Arbitrary ValidityRange where
  arbitrary =
    oneof
      [ pure Unbounded
      , MinBound <$> arbitrary
      , MaxBound <$> arbitrary
      , MinMaxBound <$> arbitrary <*> arbitrary
      ]
  shrink = genericShrink

instance Arbitrary Metadata where
  arbitrary =
    oneofStructured
      [ (Node, MetadataMap <$> listOf (resized (`div` 10) arbitrary))
      , (Node, MetadataList <$> listOf (resized (`div` 10) arbitrary))
      , (Leaf, MetadataNumber <$> arbitrary)
      , (Leaf, MetadataBytes <$> genBytes)
      , (Leaf, MetadataText . T.pack <$> arbitrary)
      ]
  shrink = \case
    MetadataMap ds -> MetadataMap <$> shrink ds
    MetadataList ds -> MetadataList <$> shrink ds
    MetadataNumber _ -> []
    MetadataBytes bytes -> MetadataBytes . BS.pack <$> shrinkList shrink (BS.unpack bytes)
    MetadataText text -> MetadataText . T.pack <$> shrinkList shrink (T.unpack text)

instance Arbitrary TransactionMetadata where
  arbitrary = TransactionMetadata <$> arbitrary
  shrink = genericShrink

instance Arbitrary TxId where
  arbitrary = TxId <$> genNBytes 32

instance Arbitrary TxIx where
  arbitrary = TxIx <$> arbitrary

instance Arbitrary TxOutRef where
  arbitrary = TxOutRef <$> arbitrary <*> arbitrary

instance Arbitrary CertIx where
  arbitrary = CertIx <$> arbitrary

instance Arbitrary Address where
  arbitrary = hedgehog $ Cardano.Api.fromCardanoAddressAny . AddressShelley <$> genAddressShelley

instance Arbitrary Assets where
  arbitrary = Assets <$> arbitrary <*> arbitrary
  shrink = genericShrink

instance Arbitrary Lovelace where
  arbitrary = Lovelace <$> arbitrary
  shrink = genericShrink

instance Arbitrary Quantity where
  arbitrary = Quantity <$> chooseWord64 (1, maxBound)
  shrink = genericShrink

instance Arbitrary Tokens where
  arbitrary = Tokens <$> arbitrary
  shrink = genericShrink

instance Arbitrary AssetId where
  arbitrary = AssetId <$> arbitrary <*> arbitrary
  shrink = genericShrink

instance Arbitrary PolicyId where
  arbitrary = PolicyId . unScriptHash <$> arbitrary

instance Arbitrary TokenName where
  arbitrary = TokenName <$> genBytes
  shrink = genericShrink

instance Arbitrary Datum where
  arbitrary =
    oneofStructured
      [ (Node, Constr . abs <$> arbitrary <*> listOf (resized (`div` 10) arbitrary))
      , (Node, Map <$> listOf (resized (`div` 10) arbitrary))
      , (Node, List <$> listOf (resized (`div` 10) arbitrary))
      , (Leaf, I <$> arbitrary)
      , (Leaf, B <$> genBytes)
      ]
  shrink = \case
    Constr i ds -> Constr i <$> shrink ds
    Map ds -> Map <$> shrink ds
    List ds -> List <$> shrink ds
    I _ -> []
    B bytes -> B . BS.pack <$> shrinkList shrink (BS.unpack bytes)

instance Arbitrary DatumHash where
  arbitrary = fromCardanoDatumHash . hashScriptDataBytes . unsafeHashableScriptData . toCardanoScriptData <$> arbitrary

instance Arbitrary Redeemer where
  arbitrary = Redeemer <$> arbitrary
  shrink = genericShrink

instance Arbitrary Credential where
  arbitrary =
    oneof
      [ PaymentKeyCredential <$> arbitrary
      , ScriptCredential <$> arbitrary
      ]
  shrink = genericShrink

instance Arbitrary StakeCredential where
  arbitrary =
    oneof
      [ StakeKeyCredential <$> arbitrary
      , StakeScriptCredential <$> arbitrary
      ]
  shrink = genericShrink

instance Arbitrary PaymentKeyHash where
  arbitrary = do
    vKey <- hedgehog $ genVerificationKey AsPaymentKey
    pure $ PaymentKeyHash $ serialiseToRawBytes $ verificationKeyHash vKey

instance Arbitrary StakeKeyHash where
  arbitrary = do
    vKey <- hedgehog $ genVerificationKey AsStakeKey
    pure $ StakeKeyHash $ serialiseToRawBytes vKey

instance Arbitrary ScriptHash where
  arbitrary = hedgehog $ fromCardanoScriptHash <$> genScriptHash

instance Arbitrary PlutusScript where
  arbitrary = do
    Shelley.PlutusScriptSerialised script <- hedgehog $ genPlutusScript PlutusScriptV2
    pure $ PlutusScript $ fromShort script

instance Arbitrary StakeReference where
  arbitrary =
    oneof
      [ StakeCredential <$> arbitrary
      , StakePointer <$> arbitrary <*> arbitrary <*> arbitrary
      ]

instance Arbitrary TxError where
  arbitrary =
    oneof
      [ pure TxNotFound
      , TxInPast <$> arbitrary
      ]

instance Arbitrary FindTxsToError where
  arbitrary = pure NoAddresses

instance Arbitrary UTxOError where
  arbitrary =
    oneof
      [ pure UTxONotFound
      , UTxOSpent <$> arbitrary
      ]

instance Arbitrary IntersectError where
  arbitrary = pure IntersectionNotFound

instance Arbitrary TransactionInput where
  arbitrary =
    TransactionInput
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
  shrink = genericShrink

instance Arbitrary TransactionOutput where
  arbitrary = do
    mDatum <- arbitrary
    TransactionOutput
      <$> arbitrary
      <*> arbitrary
      <*> maybe
        arbitrary
        (pure . Just . fromCardanoDatumHash . hashScriptDataBytes . unsafeHashableScriptData . toCardanoScriptData)
        mDatum
      <*> pure mDatum
  shrink = genericShrink

instance Arbitrary Transaction where
  arbitrary =
    Transaction
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
  shrink = genericShrink

instance Arbitrary GetUTxOsQuery where
  arbitrary =
    oneof
      [ GetUTxOsAtAddresses <$> arbitrary
      , GetUTxOsForTxOutRefs <$> arbitrary
      ]
  shrink = genericShrink

instance Arbitrary UTxOs where
  arbitrary = UTxOs <$> arbitrary
  shrink = genericShrink

instance ChainSeek.ArbitraryQuery Move where
  arbitraryTag = elements $ NE.toList ChainSeek.tags

  arbitraryQuery = \case
    TagAdvanceBlocks -> AdvanceBlocks . fromIntegral <$> arbitrary @Word64
    TagIntersect -> Intersect <$> arbitrary
    TagFindConsumingTxs -> FindConsumingTxs <$> arbitrary
    TagFindTx -> FindTx <$> arbitrary <*> arbitrary
    TagFindTxsFor -> FindTxsFor <$> (NESet.insertSet <$> arbitrary <*> arbitrary)
    TagAdvanceToTip -> pure AdvanceToTip

  arbitraryErr = \case
    TagAdvanceBlocks -> Nothing
    TagIntersect -> Just arbitrary
    TagFindConsumingTxs -> Just arbitrary
    TagFindTx -> Just arbitrary
    TagFindTxsFor -> Nothing
    TagAdvanceToTip -> Nothing

  arbitraryResult = \case
    TagAdvanceBlocks -> arbitrary
    TagIntersect -> arbitrary
    TagFindConsumingTxs -> arbitrary
    TagFindTx -> arbitrary
    TagFindTxsFor -> arbitrary
    TagAdvanceToTip -> arbitrary

  shrinkQuery = \case
    AdvanceBlocks _ -> []
    Intersect blocks -> Intersect <$> shrink blocks
    FindConsumingTxs txOuts -> FindConsumingTxs <$> shrink txOuts
    FindTx _ _ -> []
    FindTxsFor credentials -> FindTxsFor <$> mapMaybe NESet.nonEmptySet (shrink $ NESet.toSet credentials)
    AdvanceToTip -> pure AdvanceToTip

  shrinkErr = \case
    TagAdvanceBlocks -> absurd
    TagIntersect -> shrink
    TagFindConsumingTxs -> shrink
    TagFindTx -> shrink
    TagFindTxsFor -> absurd
    TagAdvanceToTip -> absurd

  shrinkResult = \case
    TagAdvanceBlocks -> shrink
    TagIntersect -> shrink
    TagFindConsumingTxs -> shrink
    TagFindTx -> shrink
    TagFindTxsFor -> shrink
    TagAdvanceToTip -> shrink

instance ChainSeek.QueryEq Move where
  queryEq = \case
    AdvanceBlocks blocks -> \case
      AdvanceBlocks blocks' -> blocks == blocks'
      _ -> False
    Intersect blocks -> \case
      Intersect blocks' -> blocks == blocks'
    FindConsumingTxs txOuts -> \case
      FindConsumingTxs txOuts' -> txOuts == txOuts'
    FindTx wait txId -> \case
      FindTx wait' txId' -> wait == wait' && txId == txId'
    FindTxsFor credentials -> \case
      FindTxsFor credentials' -> credentials == credentials'
    AdvanceToTip -> \case
      AdvanceToTip -> True
      _ -> False

  errEq = \case
    TagAdvanceBlocks -> (==)
    TagIntersect -> (==)
    TagFindConsumingTxs -> (==)
    TagFindTx -> (==)
    TagFindTxsFor -> (==)
    TagAdvanceToTip -> (==)

  resultEq = \case
    TagAdvanceBlocks -> (==)
    TagIntersect -> (==)
    TagFindConsumingTxs -> (==)
    TagFindTx -> (==)
    TagFindTxsFor -> (==)
    TagAdvanceToTip -> (==)

instance Arbitrary AnyCardanoEra where
  arbitrary =
    elements
      [ AnyCardanoEra ByronEra
      , AnyCardanoEra ShelleyEra
      , AnyCardanoEra AllegraEra
      , AnyCardanoEra MaryEra
      , AnyCardanoEra AlonzoEra
      , AnyCardanoEra BabbageEra
      , AnyCardanoEra ConwayEra
      ]

instance Query.ArbitraryRequest ChainSyncQuery where
  arbitraryTag =
    elements
      [ Query.SomeTag TagGetSecurityParameter
      , Query.SomeTag TagGetNetworkId
      , Query.SomeTag TagGetProtocolParameters
      , Query.SomeTag TagGetSystemStart
      , Query.SomeTag TagGetEraHistory
      , Query.SomeTag TagGetUTxOs
      ]

  arbitraryReq = \case
    TagGetSecurityParameter -> pure GetSecurityParameter
    TagGetNetworkId -> pure GetNetworkId
    TagGetProtocolParameters -> pure GetProtocolParameters
    TagGetSystemStart -> pure GetSystemStart
    TagGetEraHistory -> pure GetEraHistory
    TagGetUTxOs -> GetUTxOs <$> arbitrary
    TagGetNodeTip -> pure GetNodeTip
    TagGetTip -> pure GetTip
    TagGetEra -> pure GetEra

  arbitraryResult = \case
    TagGetSecurityParameter -> arbitrary
    TagGetNetworkId -> arbitrary
    TagGetProtocolParameters -> hedgehog genProtocolParameters
    TagGetSystemStart -> SystemStart . posixSecondsToUTCTime . fromIntegral <$> arbitrary @Word64
    TagGetEraHistory -> genEraHistory
    TagGetUTxOs -> arbitrary
    TagGetNodeTip -> arbitrary
    TagGetTip -> arbitrary
    TagGetEra -> arbitrary

  shrinkReq = \case
    GetSecurityParameter -> []
    GetNetworkId -> []
    GetProtocolParameters -> []
    GetSystemStart -> []
    GetEraHistory -> []
    GetUTxOs query -> GetUTxOs <$> shrink query
    GetNodeTip -> []
    GetTip -> []
    GetEra -> []

  shrinkResult = \case
    TagGetSecurityParameter -> shrink
    TagGetNetworkId -> \case
      Mainnet -> []
      Testnet _ -> [Mainnet]
    TagGetProtocolParameters -> const []
    TagGetSystemStart -> const []
    TagGetEraHistory -> const []
    TagGetUTxOs -> shrink
    TagGetNodeTip -> shrink
    TagGetTip -> shrink
    TagGetEra -> shrink

genEraHistory :: Gen (EraHistory CardanoMode)
genEraHistory =
  EraHistory CardanoMode <$> do
    byronSummary <- genEraSummary
    shelleySummary <- genEraSummary
    allegraSummary <- genEraSummary
    marySummary <- genEraSummary
    alonzoSummary <- genEraSummary
    babbageSummary <- genEraSummary
    conwaySummary <- genEraSummary
    pure $
      mkInterpreter $
        summaryWithExactly $
          Exactly $
            K byronSummary
              :* K shelleySummary
              :* K allegraSummary
              :* K marySummary
              :* K alonzoSummary
              :* K babbageSummary
              :* K conwaySummary
              :* Nil

genEraSummary :: Gen EraSummary
genEraSummary = EraSummary <$> genBound <*> genEraEnd <*> genEraParams

genBound :: Gen Bound
genBound = Bound <$> genRelativeTime <*> genSlot <*> genEpoch

genRelativeTime :: Gen RelativeTime
genRelativeTime = RelativeTime . fromIntegral <$> arbitrary @Word64

genSlot :: Gen Shelley.SlotNo
genSlot = Shelley.SlotNo <$> arbitrary

genEpoch :: Gen Shelley.EpochNo
genEpoch = Shelley.EpochNo <$> arbitrary

genEraEnd :: Gen EraEnd
genEraEnd =
  oneof
    [ pure EraUnbounded
    , EraEnd <$> genBound
    ]

genEraParams :: Gen EraParams
genEraParams = EraParams <$> genEpochSize <*> genSlotLength <*> genSafeZone

genSafeZone :: Gen SafeZone
genSafeZone =
  oneof
    [ pure UnsafeIndefiniteSafeZone
    , StandardSafeZone <$> arbitrary
    ]

genEpochSize :: Gen EpochSize
genEpochSize = EpochSize <$> arbitrary

genSlotLength :: Gen SlotLength
genSlotLength = mkSlotLength . fromIntegral <$> arbitrary @Word64

instance Query.RequestEq ChainSyncQuery where
  resultEq = \case
    TagGetSecurityParameter -> (==)
    TagGetNetworkId -> (==)
    TagGetProtocolParameters -> (==)
    TagGetSystemStart -> (==)
    TagGetEraHistory -> \(EraHistory CardanoMode interpreter1) (EraHistory CardanoMode interpreter2) ->
      unInterpreter interpreter1 == unInterpreter interpreter2
    TagGetUTxOs -> (==)
    TagGetNodeTip -> (==)
    TagGetTip -> (==)
    TagGetEra -> (==)

instance Command.ArbitraryCommand ChainSyncCommand where
  arbitraryTag =
    elements
      [ Command.SomeTag $ TagSubmitTx ScriptDataInAlonzoEra
      , Command.SomeTag $ TagSubmitTx ScriptDataInBabbageEra
      , Command.SomeTag $ TagSubmitTx ScriptDataInConwayEra
      ]
  arbitraryCmd = \case
    TagSubmitTx ScriptDataInAlonzoEra -> hedgehog $ SubmitTx ScriptDataInAlonzoEra <$> genTx AlonzoEra
    TagSubmitTx ScriptDataInBabbageEra -> hedgehog $ SubmitTx ScriptDataInBabbageEra <$> genTx BabbageEra
    TagSubmitTx ScriptDataInConwayEra -> hedgehog $ SubmitTx ScriptDataInConwayEra <$> genTx ConwayEra
  arbitraryJobId = const Nothing
  arbitraryStatus = const Nothing
  arbitraryErr = \case
    TagSubmitTx _ -> Just arbitrary
  arbitraryResult = \case
    TagSubmitTx _ -> arbitrary
  shrinkCommand = \case
    SubmitTx _ _ -> []
  shrinkJobId = \case {}
  shrinkErr = \case
    TagSubmitTx _ -> shrink
  shrinkResult = \case
    TagSubmitTx _ -> shrink
  shrinkStatus = \case
    TagSubmitTx _ -> absurd

instance Command.CommandEq ChainSyncCommand where
  commandEq = \case
    SubmitTx ScriptDataInAlonzoEra tx -> \case
      SubmitTx ScriptDataInAlonzoEra tx' -> tx == tx'
      _ -> False
    SubmitTx ScriptDataInBabbageEra tx -> \case
      SubmitTx ScriptDataInBabbageEra tx' -> tx == tx'
      _ -> False
    SubmitTx ScriptDataInConwayEra tx -> \case
      SubmitTx ScriptDataInConwayEra tx' -> tx == tx'
      _ -> False
  jobIdEq = \case {}
  statusEq = \case
    TagSubmitTx _ -> absurd
  errEq = \case
    TagSubmitTx _ -> (==)
  resultEq = \case
    TagSubmitTx _ -> (==)

genNBytes :: Int -> Gen ByteString
genNBytes len = BS.pack <$> replicateM len (chooseBoundedIntegral (minBound, maxBound))

genBytes :: Gen BS.ByteString
genBytes = BS.pack <$> listOf1 (chooseBoundedIntegral (minBound, maxBound))

data StructureType
  = Leaf
  | Node

oneofStructured :: [(StructureType, Gen a)] -> Gen a
oneofStructured gens = sized \size -> frequency $ first (structuredFrequency size) <$> gens

structuredFrequency :: Int -> StructureType -> Int
structuredFrequency size = \case
  Leaf -> 1
  Node
    | size > 0 -> 1
    | otherwise -> 0

resized :: (Int -> Int) -> Gen a -> Gen a
resized f gen = sized \size -> resize (f size) gen
