module Language.Marlowe.Protocol.Common
  where

import Control.Monad (guard)
import qualified Data.ByteString as BS
import Data.Functor (($>))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (catMaybes)
import qualified Data.Text as T
import Data.Time (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Word (Word64)
import qualified Language.Marlowe.Runtime.ChainSync.Api as Chain
import qualified Language.Marlowe.Runtime.Core.Api as Core
import Test.QuickCheck hiding (shrinkMap)

genWithGenesis :: Gen a -> Gen (Chain.WithGenesis a)
genWithGenesis genA = oneof [pure Chain.Genesis, Chain.At <$> genA]

genContractId :: Gen Core.ContractId
genContractId = Core.ContractId <$> genTxOutRef

genTxOutRef :: Gen Chain.TxOutRef
genTxOutRef = Chain.TxOutRef <$> genTxId <*> genTxIx

genTxId :: Gen Chain.TxId
genTxId = Chain.TxId <$> genBytes

genTxIx :: Gen Chain.TxIx
genTxIx = Chain.TxIx <$> arbitrary

genBytes :: Gen BS.ByteString
genBytes = BS.pack <$> listOf1 arbitrary

genSomeMarloweVersion :: Gen Core.SomeMarloweVersion
genSomeMarloweVersion = pure $ Core.SomeMarloweVersion Core.MarloweV1

genBlockHeader :: Gen Chain.BlockHeader
genBlockHeader = Chain.BlockHeader <$> genSlotNo <*> genBlockHeaderHash <*> genBlockNo

genSlotNo :: Gen Chain.SlotNo
genSlotNo = Chain.SlotNo <$> arbitrary

genBlockHeaderHash :: Gen Chain.BlockHeaderHash
genBlockHeaderHash = Chain.BlockHeaderHash <$> genBytes

genBlockNo :: Gen Chain.BlockNo
genBlockNo = Chain.BlockNo <$> arbitrary

genScriptHash :: Gen Chain.ScriptHash
genScriptHash = Chain.ScriptHash <$> genBytes

genAddress :: Gen Chain.Address
genAddress = Chain.Address <$> genBytes

genAssets :: Gen Chain.Assets
genAssets = Chain.Assets <$> genLovelace <*> genTokens

genLovelace :: Gen Chain.Lovelace
genLovelace = Chain.Lovelace <$> arbitrary

genTokens :: Gen Chain.Tokens
genTokens = Chain.Tokens . Map.fromList <$> listOf ((,) <$> genAssetId <*> genQuantity)

genAssetId :: Gen Chain.AssetId
genAssetId = Chain.AssetId <$> genPolicyId <*> genTokenName

genPolicyId :: Gen Chain.PolicyId
genPolicyId = Chain.PolicyId <$> genBytes

genTokenName :: Gen Chain.TokenName
genTokenName = Chain.TokenName <$> genBytes

genQuantity :: Gen Chain.Quantity
genQuantity = Chain.Quantity <$> arbitrary

genTransactionMetadata :: Gen Chain.TransactionMetadata
genTransactionMetadata = Chain.TransactionMetadata . Map.fromList <$> listOf ((,) <$> arbitrary <*> genMetadata)

genMetadata :: Gen Chain.Metadata
genMetadata = sized \size -> oneof $ catMaybes
  [ guard (size > 0) $> (Chain.MetadataMap <$> listOf ((,) <$> resize (size `div` 2) genMetadata <*> resize (size `div` 2) genMetadata))
  , guard (size > 0) $> (Chain.MetadataList <$> listOf (resize (size `div` 2) genMetadata))
  , Just $ Chain.MetadataNumber <$> arbitrary
  , Just $ Chain.MetadataBytes <$> genBytes
  , Just $ Chain.MetadataText . T.pack <$> arbitrary
  ]

genUTCTime :: Gen UTCTime
genUTCTime = posixSecondsToUTCTime . fromIntegral <$> arbitrary @Word64

shrinkAssets :: Chain.Assets -> [Chain.Assets]
shrinkAssets Chain.Assets{..} =
  [ Chain.Assets{..} { Chain.tokens = tokens' } | tokens' <- shrinkTokens tokens ]

shrinkTokens :: Chain.Tokens -> [Chain.Tokens]
shrinkTokens Chain.Tokens{..} = Chain.Tokens <$> shrinkMap (const []) unTokens

shrinkTransactionMetadata :: Chain.TransactionMetadata -> [Chain.TransactionMetadata]
shrinkTransactionMetadata = fmap Chain.TransactionMetadata . shrinkMap shrinkMetadata . Chain.unTransactionMetadata

shrinkMetadata :: Chain.Metadata -> [Chain.Metadata]
shrinkMetadata = \case
  Chain.MetadataMap ds -> Chain.MetadataMap <$> shrinkList (shrinkTuple shrinkMetadata shrinkMetadata) ds
  Chain.MetadataList ds -> Chain.MetadataList <$> shrinkList shrinkMetadata ds
  Chain.MetadataNumber _ -> []
  Chain.MetadataBytes bytes -> Chain.MetadataBytes . BS.pack <$> shrinkList shrink (BS.unpack bytes)
  Chain.MetadataText text -> Chain.MetadataText . T.pack <$> shrinkList shrink (T.unpack text)

shrinkMap :: (a -> [a]) -> Map k a -> [Map k a]
shrinkMap f = fmap Map.fromDistinctAscList . shrinkList (\(k, a) -> (k,) <$> f a) . Map.toAscList

shrinkTuple :: (a -> [a]) -> (b -> [b]) -> (a, b) -> [(a, b)]
shrinkTuple shrinkA shrinkB (a, b) = [ (a', b) | a' <- shrinkA a ]
                                  <> [ (a, b') | b' <- shrinkB b ]
