{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE GADTs #-}

module Language.Marlowe.Runtime.ChainSync.Gen
  where

import Cardano.Api
  ( AddressAny(..)
  , AsType(..)
  , CardanoEra(..)
  , CardanoMode
  , ConsensusMode(..)
  , EraHistory(..)
  , Key(verificationKeyHash)
  , NetworkId(..)
  , NetworkMagic(..)
  , PlutusScriptVersion(..)
  , ScriptDataSupportedInEra(..)
  , SerialiseAsRawBytes(..)
  , SystemStart(..)
  , hashScriptData
  )
import qualified Cardano.Api.Shelley as Shelley
import Control.Monad (replicateM)
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.ByteString.Short (fromShort)
import Data.Foldable (fold)
import qualified Data.List.NonEmpty as NE
import Data.Maybe (mapMaybe)
import Data.SOP.Strict (K(..), NP(..))
import qualified Data.Set.NonEmpty as NESet
import qualified Data.Text as T
import Data.These (These(..))
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Void (absurd)
import Data.Word (Word64)
import GHC.Show (showSpace)
import Gen.Cardano.Api.Typed
  (genAddressShelley, genPlutusScript, genProtocolParameters, genScriptHash, genTx, genVerificationKey)
import Language.Marlowe.Runtime.Cardano.Api (fromCardanoDatumHash, toCardanoScriptData)
import qualified Language.Marlowe.Runtime.Cardano.Api as Cardano.Api
import Language.Marlowe.Runtime.ChainSync.Api
import qualified Network.Protocol.ChainSeek.Types as ChainSeek
import qualified Network.Protocol.Job.Types as Command
import qualified Network.Protocol.Query.Types as Query
import Ouroboros.Consensus.Block (EpochSize(..))
import Ouroboros.Consensus.BlockchainTime (RelativeTime(..), SlotLength(..), mkSlotLength)
import Ouroboros.Consensus.HardFork.History
  ( Bound(..)
  , EraEnd(..)
  , EraParams(..)
  , EraSummary(..)
  , Interpreter
  , SafeZone(..)
  , Summary
  , mkInterpreter
  , summaryWithExactly
  )
import Ouroboros.Consensus.Util.Counting (Exactly(..))
import Test.QuickCheck hiding (shrinkMap)
import Test.QuickCheck.Gen (chooseWord64)
import Test.QuickCheck.Hedgehog (hedgehog)
import Unsafe.Coerce (unsafeCoerce)

instance Arbitrary a => Arbitrary (WithGenesis a) where
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
  arbitrary = oneof
    [ pure Unbounded
    , MinBound <$> arbitrary
    , MaxBound <$> arbitrary
    , MinMaxBound <$> arbitrary <*> arbitrary
    ]
  shrink = genericShrink

instance Arbitrary Metadata where
  arbitrary = oneofStructured
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
  arbitrary = oneofStructured
    [ (Node, Constr <$> arbitrary <*> listOf (resized (`div` 10) arbitrary))
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
  arbitrary = fromCardanoDatumHash . hashScriptData . toCardanoScriptData <$> arbitrary

instance Arbitrary Redeemer where
  arbitrary = Redeemer <$> arbitrary
  shrink = genericShrink

instance Arbitrary Credential where
  arbitrary = oneof
    [ PaymentKeyCredential <$> arbitrary
    , ScriptCredential <$> arbitrary
    ]
  shrink = genericShrink

instance Arbitrary StakeCredential where
  arbitrary = oneof
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
  arbitrary = oneof
    [ StakeCredential <$> arbitrary
    , StakePointer <$> arbitrary <*> arbitrary <*> arbitrary
    ]

instance Arbitrary TxError where
  arbitrary = oneof
    [ pure TxNotFound
    , TxInPast <$> arbitrary
    ]

instance Arbitrary FindTxsToError where
  arbitrary = pure NoAddresses

instance Arbitrary UTxOError where
  arbitrary = oneof
    [ pure UTxONotFound
    , UTxOSpent <$> arbitrary
    ]

instance Arbitrary IntersectError where
  arbitrary = pure IntersectionNotFound

instance Arbitrary TransactionInput where
  arbitrary = TransactionInput
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
        (pure . Just . fromCardanoDatumHash . hashScriptData . toCardanoScriptData)
        mDatum
      <*> pure mDatum
  shrink = genericShrink

instance Arbitrary Transaction where
  arbitrary = Transaction
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
  shrink = genericShrink

instance Arbitrary GetUTxOsQuery where
  arbitrary = oneof
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
    AdvanceBlocks  _-> []
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

instance ChainSeek.ShowQuery Move where
  showsPrecTag _ = showString . \case
    TagAdvanceBlocks -> "TagAdvanceBlocks"
    TagIntersect -> "TagIntersect"
    TagFindConsumingTxs -> "TagFindConsumingTxs"
    TagFindTx -> "TagFindTx"
    TagFindTxsFor -> "TagFindTxsFor"
    TagAdvanceToTip -> "TagAdvanceToTip"

  showsPrecQuery p = \case
    AdvanceBlocks blocks -> showParen (p >= 11)
      ( showString "AdvanceBlocks"
      . showSpace
      . showsPrec 11 blocks
      )
    Intersect blocks -> showParen (p >= 11)
      ( showString "Intersect"
      . showSpace
      . showsPrec 11 blocks
      )
    FindConsumingTxs txOuts -> showParen (p >= 11)
      ( showString "FindConsumingTxs"
      . showSpace
      . showsPrec 11 txOuts
      )
    FindTx wait txId -> showParen (p >= 11)
      ( showString "FindTx"
      . showSpace
      . showsPrec 11 wait
      . showSpace
      . showsPrec 11 txId
      )
    FindTxsFor credentials -> showParen (p >= 11)
      ( showString "FindTxsFor"
      . showSpace
      . showsPrec 11 credentials
      )
    AdvanceToTip -> showString "AdvanceToTip"

  showsPrecErr p = \case
    TagAdvanceBlocks -> showsPrec p
    TagIntersect -> showsPrec p
    TagFindConsumingTxs -> showsPrec p
    TagFindTx -> showsPrec p
    TagFindTxsFor -> showsPrec p
    TagAdvanceToTip -> showsPrec p

  showsPrecResult p = \case
    TagAdvanceBlocks -> showsPrec p
    TagIntersect -> showsPrec p
    TagFindConsumingTxs -> showsPrec p
    TagFindTx -> showsPrec p
    TagFindTxsFor -> showsPrec p
    TagAdvanceToTip -> showsPrec p

instance Query.ArbitraryQuery ChainSyncQuery where
  arbitraryTag = elements
    [ Query.SomeTag TagGetSecurityParameter
    , Query.SomeTag TagGetNetworkId
    , Query.SomeTag TagGetProtocolParameters
    , Query.SomeTag TagGetSystemStart
    , Query.SomeTag TagGetEraHistory
    , Query.SomeTag TagGetUTxOs
    ]

  arbitraryQuery = \case
    TagGetSecurityParameter -> pure GetSecurityParameter
    TagGetNetworkId -> pure GetNetworkId
    TagGetProtocolParameters -> pure GetProtocolParameters
    TagGetSystemStart -> pure GetSystemStart
    TagGetEraHistory -> pure GetEraHistory
    TagGetUTxOs -> GetUTxOs <$> arbitrary

  arbitraryDelimiter = \case
    TagGetSecurityParameter -> Nothing
    TagGetNetworkId -> Nothing
    TagGetProtocolParameters -> Nothing
    TagGetSystemStart -> Nothing
    TagGetEraHistory -> Nothing
    TagGetUTxOs -> Nothing

  arbitraryErr = \case
    TagGetSecurityParameter -> Just arbitrary
    TagGetNetworkId -> Just arbitrary
    TagGetProtocolParameters -> Just arbitrary
    TagGetSystemStart -> Just arbitrary
    TagGetEraHistory -> Just arbitrary
    TagGetUTxOs -> Just arbitrary

  arbitraryResults = \case
    TagGetSecurityParameter -> arbitrary
    TagGetNetworkId -> oneof
      [ pure Mainnet
      , Testnet . NetworkMagic <$> arbitrary
      ]
    TagGetProtocolParameters -> hedgehog genProtocolParameters
    TagGetSystemStart -> SystemStart . posixSecondsToUTCTime . fromIntegral <$> arbitrary @Word64
    TagGetEraHistory -> genEraHistory
    TagGetUTxOs -> arbitrary

  shrinkQuery = \case
    GetSecurityParameter -> []
    GetNetworkId -> []
    GetProtocolParameters -> []
    GetSystemStart -> []
    GetEraHistory -> []
    GetUTxOs query -> GetUTxOs <$> shrink query

  shrinkErr = \case
    TagGetSecurityParameter -> shrink
    TagGetNetworkId -> shrink
    TagGetProtocolParameters -> shrink
    TagGetSystemStart -> shrink
    TagGetEraHistory -> shrink
    TagGetUTxOs -> shrink

  shrinkResults = \case
    TagGetSecurityParameter -> shrink
    TagGetNetworkId -> \case
      Mainnet -> []
      Testnet _ -> [Mainnet]
    TagGetProtocolParameters -> const []
    TagGetSystemStart -> const []
    TagGetEraHistory -> const []
    TagGetUTxOs -> shrink

  shrinkDelimiter = \case
    TagGetSecurityParameter -> absurd
    TagGetNetworkId -> absurd
    TagGetProtocolParameters -> absurd
    TagGetSystemStart -> absurd
    TagGetEraHistory -> absurd
    TagGetUTxOs -> absurd

genEraHistory :: Gen (EraHistory CardanoMode)
genEraHistory = EraHistory CardanoMode <$> do
  byronSummary <- genEraSummary
  shelleySummary <- genEraSummary
  allegraSummary <- genEraSummary
  marySummary <- genEraSummary
  alonzoSummary <- genEraSummary
  babbageSummary <- genEraSummary
  pure $ mkInterpreter $ summaryWithExactly $ Exactly
    $  K byronSummary
    :* K shelleySummary
    :* K allegraSummary
    :* K marySummary
    :* K alonzoSummary
    :* K babbageSummary
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
genEraEnd = oneof
  [ pure EraUnbounded
  , EraEnd <$> genBound
  ]

genEraParams :: Gen EraParams
genEraParams = EraParams <$> genEpochSize <*> genSlotLength <*> genSafeZone

genSafeZone :: Gen SafeZone
genSafeZone = oneof
  [ pure UnsafeIndefiniteSafeZone
  , StandardSafeZone <$> arbitrary
  ]

genEpochSize :: Gen EpochSize
genEpochSize = EpochSize <$> arbitrary

genSlotLength :: Gen SlotLength
genSlotLength = mkSlotLength . fromIntegral <$> arbitrary @Word64

instance Query.QueryEq ChainSyncQuery where
  queryEq = \case
    GetSecurityParameter -> \case
      GetSecurityParameter -> True
    GetNetworkId -> \case
      GetNetworkId -> True
    GetProtocolParameters -> \case
      GetProtocolParameters -> True
    GetSystemStart -> \case
      GetSystemStart -> True
    GetEraHistory -> \case
      GetEraHistory -> True
    GetUTxOs query -> \case
      GetUTxOs query' -> query == query'
  delimiterEq = \case
    TagGetSecurityParameter -> absurd
    TagGetNetworkId -> absurd
    TagGetProtocolParameters -> absurd
    TagGetSystemStart -> absurd
    TagGetEraHistory -> absurd
    TagGetUTxOs -> absurd
  errEq = \case
    TagGetSecurityParameter -> (==)
    TagGetNetworkId -> (==)
    TagGetProtocolParameters -> (==)
    TagGetSystemStart -> (==)
    TagGetEraHistory -> (==)
    TagGetUTxOs -> (==)
  resultEq = \case
    TagGetSecurityParameter -> (==)
    TagGetNetworkId -> (==)
    TagGetProtocolParameters -> (==)
    TagGetSystemStart -> (==)
    TagGetEraHistory -> \(EraHistory CardanoMode interpreter1) (EraHistory CardanoMode interpreter2) ->
      unInterpreter interpreter1 == unInterpreter interpreter2
    TagGetUTxOs -> (==)

unInterpreter :: Interpreter xs -> Summary xs
unInterpreter = unsafeCoerce

instance Query.ShowQuery ChainSyncQuery where
  showsPrecTag _ = showString . \case
    TagGetSecurityParameter -> "TagGetSecurityParameter"
    TagGetNetworkId -> "TagGetNetworkId"
    TagGetProtocolParameters -> "TagGetProtocolParameters"
    TagGetSystemStart -> "TagGetSystemStart"
    TagGetEraHistory -> "TagGetEraHistory"
    TagGetUTxOs -> "TagGetUTxOs"

  showsPrecQuery p = \case
    GetSecurityParameter -> showString "GetSecurityParameter"
    GetNetworkId -> showString "GetNetworkId"
    GetProtocolParameters -> showString "GetProtocolParameters"
    GetSystemStart -> showString "GetSystemStart"
    GetEraHistory -> showString "GetEraHistory"
    GetUTxOs query -> showParen (p >= 11)
      ( showString "TagGetUTxOs"
      . showSpace
      . showsPrec 11 query
      )

  showsPrecDelimiter _ = \case
    TagGetSecurityParameter -> absurd
    TagGetNetworkId -> absurd
    TagGetProtocolParameters -> absurd
    TagGetSystemStart -> absurd
    TagGetEraHistory -> absurd
    TagGetUTxOs -> absurd

  showsPrecErr p = \case
    TagGetSecurityParameter -> showsPrec p
    TagGetNetworkId -> showsPrec p
    TagGetProtocolParameters -> showsPrec p
    TagGetSystemStart -> showsPrec p
    TagGetEraHistory -> showsPrec p
    TagGetUTxOs -> showsPrec p

  showsPrecResult p = \case
    TagGetSecurityParameter -> showsPrec p
    TagGetNetworkId -> showsPrec p
    TagGetProtocolParameters -> showsPrec p
    TagGetSystemStart -> showsPrec p
    TagGetEraHistory -> \(EraHistory CardanoMode interpreter) -> showParen (p >= 11)
      ( showString "EraHistory"
      . showSpace
      . showString "CardanoMode"
      . showSpace
      . showParen True
        ( showString "mkInterpreter"
        . showSpace
        . showsPrec 11 (unInterpreter interpreter)
        )
      )
    TagGetUTxOs -> showsPrec p

instance Command.ArbitraryCommand ChainSyncCommand where
  arbitraryTag = elements
    [ Command.SomeTag $ TagSubmitTx ScriptDataInAlonzoEra
    , Command.SomeTag $ TagSubmitTx ScriptDataInBabbageEra
    ]
  arbitraryCmd = \case
    TagSubmitTx ScriptDataInAlonzoEra -> hedgehog $ SubmitTx ScriptDataInAlonzoEra <$> genTx AlonzoEra
    TagSubmitTx ScriptDataInBabbageEra -> hedgehog $ SubmitTx ScriptDataInBabbageEra <$> genTx BabbageEra
  arbitraryJobId = const Nothing
  arbitraryStatus = const Nothing
  arbitraryErr = \case
    TagSubmitTx _ -> Just arbitrary
  arbitraryResult = \case
    TagSubmitTx _ -> arbitrary
  shrinkCommand = \case
    SubmitTx _ _ -> []
  shrinkJobId = \case
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
  jobIdEq = \case
  statusEq = \case
    TagSubmitTx _ -> absurd
  errEq = \case
    TagSubmitTx _ -> (==)
  resultEq = \case
    TagSubmitTx _ -> (==)

instance Command.ShowCommand ChainSyncCommand where
  showsPrecTag p = showParen (p >= 11) . showString . \case
    TagSubmitTx ScriptDataInAlonzoEra -> "TagSubmitTx ScriptDataInAlonzoEra"
    TagSubmitTx ScriptDataInBabbageEra -> "TagSubmitTx ScriptDataInBabbageEra"
  showsPrecCommand p = showParen (p >= 11) . \case
    SubmitTx ScriptDataInAlonzoEra tx ->
      ( showString "TagSubmitTx ScriptDataInAlonzoEra"
      . showSpace
      . showsPrec p tx
      )
    SubmitTx ScriptDataInBabbageEra tx ->
      ( showString "TagSubmitTx ScriptDataInBabbageEra"
      . showSpace
      . showsPrec p tx
      )
  showsPrecJobId _ = \case
  showsPrecStatus _ = \case
    TagSubmitTx _ -> absurd
  showsPrecErr p = \case
    TagSubmitTx _ -> showsPrec p
  showsPrecResult p = \case
    TagSubmitTx _ -> showsPrec p

showsPrecThese :: Int -> (Int -> a -> ShowS) -> (Int -> b -> ShowS) -> These a b -> ShowS
showsPrecThese p showsPrecA showsPrecB = showParen (p >= 11) . \case
  This a -> (showString "This" . showSpace . showsPrecA 11 a)
  That b -> (showString "That" . showSpace . showsPrecB 11 b)
  These a b ->
    ( showString "These"
    . showSpace
    . showsPrecA 11 a
    . showSpace
    . showsPrecB 11 b
    )

theseEq :: (a -> a -> Bool) -> (b -> b -> Bool) -> These a b -> These a b -> Bool
theseEq eqA eqB = \case
  This a -> \case
    This a' -> eqA a a'
    _ -> False
  That b -> \case
    That b' -> eqB b b'
    _ -> False
  These a b -> \case
    These a' b' -> eqA a a' == eqB b b'
    _ -> False


genThese :: Gen a -> Gen b -> Gen (These a b)
genThese a b = oneof [This <$> a, That <$> b, resized (`div` 2) $ These <$> a <*> b]

shrinkThese :: (a -> [a]) -> (b -> [b]) -> These a b -> [These a b]
shrinkThese shrinkA shrinkB = \case
  This a -> This <$> shrinkA a
  That b -> That <$> shrinkB b
  These a b -> This a : That b : fold
    [ [ These a' b | a' <- shrinkA a ]
    , [ These a b' | b' <- shrinkB b ]
    ]

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
