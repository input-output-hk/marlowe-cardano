{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Language.Marlowe.Runtime.Sync.Database.PostgreSQL.GetHeaders where

import Control.Foldl (Fold)
import qualified Control.Foldl as Fold
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT (..))
import Data.Binary (get)
import Data.Binary.Get (runGet)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (fromStrict)
import Data.Int (Int16, Int32, Int64)
import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromJust)
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Vector as Vector
import qualified Hasql.Decoders as Decoders
import Hasql.DynamicSyntax.Ast
import Hasql.DynamicSyntax.Schema (tableColumn)
import Hasql.DynamicSyntax.Statement (StatementBuilder, buildStatement, param)
import Hasql.Statement (Statement)
import qualified Hasql.Transaction as T
import Language.Marlowe.Protocol.Query.Types
import Language.Marlowe.Runtime.ChainSync.Api (
  Address (..),
  BlockHeader (..),
  BlockHeaderHash (..),
  Credential (..),
  PolicyId (..),
  ScriptHash (..),
  TxId (..),
  TxOutRef (..),
  paymentCredential,
 )
import Language.Marlowe.Runtime.Core.Api (
  ContractId (..),
  MarloweMetadataTag (getMarloweMetadataTag),
  MarloweVersion (MarloweV1),
  SomeMarloweVersion (SomeMarloweVersion),
  emptyMarloweTransactionMetadata,
 )
import Language.Marlowe.Runtime.Discovery.Api (ContractHeader (..))
import Language.Marlowe.Runtime.Schema (equals, naturalJoin)
import qualified Language.Marlowe.Runtime.Schema as Schema
import Language.Marlowe.Runtime.Sync.Database.PostgreSQL.GetPayouts (
  countAll,
  laxComparisonCond,
  rangeSortBy,
  strictComparisonCond,
 )
import PostgresqlSyntax.Ast (AliasClause (..))
import Prelude hiding (init)

getHeaders
  :: ContractFilter
  -> Range ContractId
  -> T.Transaction (Maybe (Page ContractId ContractHeader))
getHeaders _ Range{..}
  -- Invalid requests. Note rangeLimit == 0 is invalid because it produces no
  -- results and causes infinite paging, which is potentially dangerous for
  -- clients as they could get caught in an infinite loop if consuming all
  -- pages.
  | rangeLimit <= 0 || rangeOffset < 0 = pure Nothing
getHeaders cFilter range@Range{..} = runMaybeT do
  mDelimiter <- traverse (MaybeT . T.statement () . delimiterStatement cFilter) rangeStart
  lift do
    totalCount <- T.statement () $ totalCountStatement cFilter
    itemsWithNext <- T.statement () $ headersStatement cFilter range mDelimiter
    let nextRange = case drop rangeLimit itemsWithNext of
          [] -> Nothing
          ContractHeader{..} : _ -> Just Range{rangeStart = Just contractId, rangeOffset = 0, ..}
        items = take rangeLimit itemsWithNext
    pure Page{..}

foldPage :: (row -> id) -> (row -> item) -> Int -> Order -> Int -> Fold row (Page id item)
foldPage decodeId decodeItem limit direction totalCount =
  Page
    <$> foldItems decodeItem limit
    <*> foldNextRange decodeId limit direction
    <*> pure totalCount

foldItems :: (row -> item) -> Int -> Fold row [item]
foldItems decodeItem limit = fmap decodeItem . take limit <$> Fold.list

decodeContractHeader
  :: Int64
  -> ByteString
  -> Int64
  -> ByteString
  -> Int16
  -> ByteString
  -> Maybe ByteString
  -> ByteString
  -> ByteString
  -> ContractHeader
decodeContractHeader slotNo blockHeaderHash blockNo txId txIx rolesCurrency metadata address payoutScriptHash =
  ContractHeader
    { contractId = ContractId (TxOutRef (TxId txId) (fromIntegral txIx))
    , rolesCurrency = PolicyId rolesCurrency
    , metadata = maybe emptyMarloweTransactionMetadata (runGet get . fromStrict) metadata
    , marloweScriptHash = fromJust do
        credential <- paymentCredential $ Address address
        case credential of
          ScriptCredential hash -> pure hash
          _ -> Nothing
    , marloweScriptAddress = Address address
    , payoutScriptHash = ScriptHash payoutScriptHash
    , marloweVersion = SomeMarloweVersion MarloweV1
    , blockHeader =
        BlockHeader
          (fromIntegral slotNo)
          (BlockHeaderHash blockHeaderHash)
          (fromIntegral blockNo)
    }

foldNextRange :: (row -> id) -> Int -> Order -> Fold row (Maybe (Range id))
foldNextRange decodeId rangeLimit rangeDirection = do
  mNext <- Fold.index rangeLimit
  pure do
    row <- mNext
    pure
      Range
        { rangeStart = Just $ decodeId row
        , rangeOffset = 0
        , ..
        }

data DelimiterRow = DelimiterRow
  { delimiterTxId :: ByteString
  , delimiterTxIx :: Int16
  , delimiterSlotNo :: Int64
  }

-- | A select statement which looks for the delimiter specified by the given contractId.
delimiterStatement :: ContractFilter -> ContractId -> Statement () (Maybe DelimiterRow)
delimiterStatement cFilter (ContractId TxOutRef{..}) = buildStatement DelimiterRow Decoders.rowMaybe do
  -- Allocate the tables.
  fromClause <- delimiterTables cFilter
  -- Allocate parameters for the delimiter.
  txIdParam <- param $ unTxId txId
  txIxParam <- param @Int16 $ fromIntegral txIx
  whereClause <- filterCondition cFilter $ Just $ delimiterIdCond txIdParam txIxParam
  let selectClause =
        NormalSimpleSelect
          ( NormalTargeting $
              tableColumn @"txId" Schema.createTxOut
                :. tableColumn @"txIx" Schema.createTxOut
                :. tableColumn @"slotNo" Schema.createTxOut
                :. TargetListNil
          )
          Nothing
          (Just $ pure fromClause)
          whereClause
          Nothing
          Nothing
          Nothing
  pure $ SelectPreparableStmt $ Left $ SelectNoParens Nothing (Left selectClause) Nothing Nothing Nothing

-- | A select statement which looks for the delimiter specified by the given contractId.
totalCountStatement :: ContractFilter -> Statement () Int
totalCountStatement cFilter = buildStatement fromIntegral Decoders.singleRow do
  -- Allocate the tables.
  fromClause <- delimiterTables cFilter
  whereClause <- filterCondition cFilter Nothing
  let selectClause =
        NormalSimpleSelect
          (NormalTargeting $ countAll :. TargetListNil)
          Nothing
          (Just $ pure fromClause)
          whereClause
          Nothing
          Nothing
          Nothing
  pure $ SelectPreparableStmt $ Left $ SelectNoParens Nothing (Left selectClause) Nothing Nothing Nothing

headersStatement :: ContractFilter -> Range ContractId -> Maybe DelimiterRow -> Statement () [ContractHeader]
headersStatement cFilter Range{..} mDelimiter = buildStatement decodeContractHeader Decoders.rowList do
  -- Allocate the tables.
  fromClause <- headersTables cFilter
  -- Allocate the where clause
  whereClause <- filterCondition cFilter =<< traverse (delimiterComparisonCond rangeDirection) mDelimiter
  -- Allocate limit and offset params
  offsetParam <- param $ fromIntegral @_ @Int32 rangeOffset
  limitParam <- param $ fromIntegral @_ @Int32 (rangeLimit + 1)
  let selectClause =
        NormalSimpleSelect
          ( NormalTargeting $
              tableColumn @"slotNo" Schema.createTxOut
                :. tableColumn @"blockId" Schema.createTxOut
                :. tableColumn @"blockNo" Schema.createTxOut
                :. tableColumn @"txId" Schema.createTxOut
                :. tableColumn @"txIx" Schema.createTxOut
                :. tableColumn @"rolesCurrency" Schema.contractTxOut
                :. tableColumn @"metadata" Schema.createTxOut
                :. tableColumn @"address" Schema.txOut
                :. tableColumn @"payoutScriptHash" Schema.contractTxOut
                :. TargetListNil
          )
          Nothing
          (Just $ pure fromClause)
          whereClause
          Nothing
          Nothing
          Nothing
      sortClause =
        NE.fromList
          [ rangeSortBy rangeDirection $ tableColumn @"slotNo" Schema.createTxOut
          , rangeSortBy rangeDirection $ tableColumn @"txId" Schema.createTxOut
          , rangeSortBy rangeDirection $ tableColumn @"txIx" Schema.createTxOut
          ]
      selectLimit =
        OffsetLimitSelectLimit offsetParam $
          FetchOnlyLimitClause True (Just $ toSelectFetchFirstValue limitParam) True
  pure $
    SelectPreparableStmt $
      Left $
        SelectNoParens Nothing (Left selectClause) (Just sortClause) (Just selectLimit) Nothing

-- * Tables

delimiterTables :: ContractFilter -> StatementBuilder TableRef
delimiterTables ContractFilter{..}
  | Set.null roleCurrencies = pure $ toTableRef Schema.createTxOut
  | otherwise = do
      rolesTableRef <- rolesTable roleCurrencies
      pure $
        Schema.createTxOut
          `naturalJoin` Schema.contractTxOut
          `naturalJoin` rolesTableRef

headersTables :: ContractFilter -> StatementBuilder TableRef
headersTables ContractFilter{..}
  | Set.null roleCurrencies =
      pure $
        Schema.createTxOut
          `naturalJoin` Schema.contractTxOut
          `naturalJoin` Schema.txOut
  | otherwise = do
      rolesTableRef <- rolesTable roleCurrencies
      pure $
        Schema.createTxOut
          `naturalJoin` Schema.contractTxOut
          `naturalJoin` Schema.txOut
          `naturalJoin` rolesTableRef

rolesTable :: Set.Set PolicyId -> StatementBuilder TableRef
rolesTable roleCurrencies = do
  roleCurrenciesParam <- param $ Vector.fromList $ unPolicyId <$> Set.toList roleCurrencies
  let unnestApplication =
        FuncApplication "UNNEST" $
          Just $
            NormalFuncApplicationParams Nothing (pure $ toFuncArgExpr roleCurrenciesParam) Nothing
      unnestReturnTypes =
        TargetTypesCons
          (ColumnType (SqlArray (ColumnType SqlBytea NotNull)) NotNull)
          TargetTypesNil
      selectClause =
        NormalSimpleSelect
          ( NormalTargeting $
              TargetElRow unnestReturnTypes (ApplicationFuncExpr unnestApplication Nothing Nothing Nothing)
                :. TargetListNil
          )
          Nothing
          Nothing
          Nothing
          Nothing
          Nothing
          Nothing
  pure $
    SelectTableRef False (NoParensSelectWithParens $ SelectNoParens Nothing (Left selectClause) Nothing Nothing Nothing) $
      Just $
        AliasClause True "roles" $
          Just $
            pure "rolesCurrency"

-- * Conditions

delimiterIdCond :: Param -> Param -> AExpr
delimiterIdCond txIdParam txIxParam = do
  AndAExpr
    (tableColumn @"txId" Schema.createTxOut `equals` txIdParam)
    (tableColumn @"txIx" Schema.createTxOut `equals` txIxParam)

delimiterComparisonCond :: Order -> DelimiterRow -> StatementBuilder AExpr
delimiterComparisonCond order DelimiterRow{..} = do
  slotNoParam <- param delimiterSlotNo
  txIdParam <- param delimiterTxId
  txIxParam <- param delimiterTxIx
  pure $
    CExprAExpr $
      flip InParensCExpr Nothing $
        OrAExpr
          (strictComparisonCond order (tableColumn @"slotNo" Schema.createTxOut) slotNoParam)
          ( flip InParensCExpr Nothing $
              AndAExpr
                (tableColumn @"slotNo" Schema.createTxOut `equals` slotNoParam)
                ( flip InParensCExpr Nothing $
                    OrAExpr
                      (strictComparisonCond order (tableColumn @"txId" Schema.createTxOut) txIdParam)
                      ( flip InParensCExpr Nothing $
                          AndAExpr
                            (tableColumn @"txId" Schema.createTxOut `equals` txIdParam)
                            (laxComparisonCond order (tableColumn @"txIx" Schema.createTxOut) txIxParam)
                      )
                )
          )

-- | Adds additional checks to a condition as required by the contract filter.
filterCondition :: ContractFilter -> Maybe AExpr -> StatementBuilder (Maybe AExpr)
filterCondition ContractFilter{..} otherChecks
  | Set.null tags = pure otherChecks
  | otherwise = do
      tagExists <- tagExistsCond tags
      pure $ Just $ maybe tagExists (`AndAExpr` tagExists) otherChecks

tagExistsCond :: Set MarloweMetadataTag -> StatementBuilder AExpr
tagExistsCond tags = do
  tagsParam <- param $ Vector.fromList $ getMarloweMetadataTag <$> Set.toList tags
  let selectClause =
        NormalSimpleSelect
          targeting
          Nothing
          (Just $ pure fromClause)
          (Just whereClause)
          Nothing
          Nothing
          Nothing
      targeting = NormalTargeting $ TargetElRow returnTypes (IAexprConst 1) :. TargetListNil
      returnTypes = TargetTypesCons (ColumnType SqlInt4 NotNull) TargetTypesNil
      tagTable =
        FuncTableRef
          False
          ( FuncExprFuncTable
              ( ApplicationFuncExprWindowless $
                  FuncApplication "UNNEST" $
                    Just $
                      NormalFuncApplicationParams Nothing (pure $ toFuncArgExpr tagsParam) Nothing
              )
              False
          )
          (Just $ AliasFuncAliasClause $ AliasClause True "tags" $ Just $ pure "tag")

      fromClause = Schema.contractTxOutTag `naturalJoin` tagTable
      whereClause =
        AndAExpr
          (tableColumn @"txId" Schema.contractTxOutTag `equals` tableColumn @"txId" Schema.createTxOut)
          (tableColumn @"txIx" Schema.contractTxOutTag `equals` tableColumn @"txIx" Schema.createTxOut)
  pure $
    CExprAExpr $
      ExistsCExpr $
        NoParensSelectWithParens $
          SelectNoParens Nothing (Left selectClause) Nothing Nothing Nothing
