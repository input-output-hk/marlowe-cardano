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
import Data.Coerce (coerce)
import Data.Int (Int16, Int32, Int64)
import qualified Data.List.NonEmpty as NE
import Data.Maybe (catMaybes, fromJust)
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Vector as Vector
import qualified Hasql.Decoders as Decoders
import Hasql.DynamicSyntax.Ast
import Hasql.DynamicSyntax.Schema (Table (..), cte, tableColumn, wildcard)
import Hasql.DynamicSyntax.Statement (StatementBuilder, buildStatement, param)
import Hasql.Statement (Statement)
import qualified Hasql.Transaction as T
import Language.Marlowe.Protocol.Query.Types
import Language.Marlowe.Runtime.ChainSync.Api (
  Address (..),
  AssetId (..),
  BlockHeader (..),
  BlockHeaderHash (..),
  Credential (..),
  PolicyId (..),
  ScriptHash (..),
  TokenName (..),
  TxId (..),
  TxIx (TxIx),
  TxOutRef (..),
  paymentCredential,
  unTxIx,
 )
import Language.Marlowe.Runtime.Core.Api (
  ContractId (..),
  MarloweMetadataTag (getMarloweMetadataTag),
  MarloweVersion (MarloweV1),
  SomeMarloweVersion (SomeMarloweVersion),
  emptyMarloweTransactionMetadata,
 )
import Language.Marlowe.Runtime.Discovery.Api (ContractHeader (..))
import Language.Marlowe.Runtime.Schema (countAll, equals, existsCond, naturalJoin, unnestParams, withCTEs)
import qualified Language.Marlowe.Runtime.Schema as Schema
import Language.Marlowe.Runtime.Sync.Database.PostgreSQL.GetPayouts (
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
    { contractId = ContractId (TxOutRef (TxId txId) (TxIx $ fromIntegral txIx))
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
  (withClause, fromClause) <- delimiterTables cFilter
  -- Allocate parameters for the delimiter.
  txIdParam <- param $ unTxId txId
  txIxParam <- param @Int16 $ fromIntegral $ unTxIx txIx
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
  pure $ SelectPreparableStmt $ Left $ SelectNoParens withClause (Left selectClause) Nothing Nothing Nothing

-- | A select statement which looks for the delimiter specified by the given contractId.
totalCountStatement :: ContractFilter -> Statement () Int
totalCountStatement cFilter = buildStatement fromIntegral Decoders.singleRow do
  -- Allocate the tables.
  (withClause, fromClause) <- delimiterTables cFilter
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
  pure $ SelectPreparableStmt $ Left $ SelectNoParens withClause (Left selectClause) Nothing Nothing Nothing

headersStatement :: ContractFilter -> Range ContractId -> Maybe DelimiterRow -> Statement () [ContractHeader]
headersStatement cFilter Range{..} mDelimiter = buildStatement decodeContractHeader Decoders.rowList do
  -- Allocate the tables.
  (withClause, fromClause) <- headersTables cFilter
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
        SelectNoParens withClause (Left selectClause) (Just sortClause) (Just selectLimit) Nothing

-- * Tables

-- | The columns of the partyAddresses CTE
type PartyAddressesColumns =
  '[ '("address", SqlBytea, NotNull)
   ]

-- | The columns of the partyRoles CTE
type PartyRolesColumns =
  '[ '("rolesCurrency", SqlBytea, NotNull)
   , '("role", SqlBytea, NotNull)
   ]

-- | The partyAddresses CTE table
partyAddressesTable :: Table PartyAddressesColumns
partyAddressesTable = Schema.tempTable "partyAddresses"

-- | The partyRoles CTE table
partyRolesTable :: Table PartyRolesColumns
partyRolesTable = Schema.tempTable "partyRoles"

-- |
--  ==== SQL
--  @
--  partyAddresses (address)
--     ( SELECT * FROM UNNEST ($1)
--     )
--  @
partyAddressesCTE :: Set Address -> StatementBuilder (Maybe CommonTableExpr)
partyAddressesCTE addresses
  | Set.null addresses = pure Nothing
  | otherwise = do
      addressesParam <- param $ Vector.fromList $ coerce @_ @[ByteString] $ Set.toList addresses
      pure $
        Just $
          cte partyAddressesTable . simpleSelectPreparableStmt $
            NormalSimpleSelect
              (wildcard partyAddressesTable)
              Nothing
              (Just $ pure $ unnestParams (pure addressesParam) Nothing)
              Nothing
              Nothing
              Nothing
              Nothing

-- |
--  ==== SQL
--  @
--  partyRoles (rolesCurrency, role)
--     ( SELECT * FROM UNNEST ($1, $2)
--     )
--  @
partyRolesCTE :: Set AssetId -> StatementBuilder (Maybe CommonTableExpr)
partyRolesCTE roles
  | Set.null roles = pure Nothing
  | otherwise = do
      let rolesVector = Vector.fromList $ Set.toList roles
      policyIdsParam <- param $ unPolicyId . policyId <$> rolesVector
      tokenNameParam <- param $ unTokenName . tokenName <$> rolesVector
      pure $
        Just $
          cte partyRolesTable . simpleSelectPreparableStmt $
            NormalSimpleSelect
              (wildcard partyRolesTable)
              Nothing
              (Just $ pure $ unnestParams (NE.fromList [policyIdsParam, tokenNameParam]) Nothing)
              Nothing
              Nothing
              Nothing
              Nothing

delimiterTables :: ContractFilter -> StatementBuilder (Maybe WithClause, TableRef)
delimiterTables ContractFilter{..} = do
  mPartyAddressesCTE <- partyAddressesCTE partyAddresses
  mPartyRolesCTE <- partyRolesCTE partyRoles
  (withCTEs False $ catMaybes [mPartyAddressesCTE, mPartyRolesCTE],)
    <$> if Set.null roleCurrencies
      then pure $ toTableRef Schema.createTxOut
      else do
        rolesTableRef <- rolesTable roleCurrencies
        pure $
          Schema.createTxOut
            `naturalJoin` Schema.contractTxOut
            `naturalJoin` rolesTableRef

headersTables :: ContractFilter -> StatementBuilder (Maybe WithClause, TableRef)
headersTables ContractFilter{..} = do
  mPartyAddressesCTE <- partyAddressesCTE partyAddresses
  mPartyRolesCTE <- partyRolesCTE partyRoles
  (withCTEs False $ catMaybes [mPartyAddressesCTE, mPartyRolesCTE],)
    <$> if Set.null roleCurrencies
      then
        pure $
          Schema.createTxOut
            `naturalJoin` Schema.contractTxOut
            `naturalJoin` Schema.txOut
      else do
        rolesTableRef <- rolesTable roleCurrencies
        pure $
          Schema.createTxOut
            `naturalJoin` Schema.contractTxOut
            `naturalJoin` Schema.txOut
            `naturalJoin` rolesTableRef

rolesTable :: Set.Set PolicyId -> StatementBuilder TableRef
rolesTable roleCurrencies = do
  roleCurrenciesParam <- param $ Vector.fromList $ unPolicyId <$> Set.toList roleCurrencies
  pure $
    unnestParams (pure roleCurrenciesParam) $
      Just $
        AliasFuncAliasClause $
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
    OrAExpr
      (strictComparisonCond order (tableColumn @"slotNo" Schema.createTxOut) slotNoParam)
      ( parenthesize $
          AndAExpr
            (tableColumn @"slotNo" Schema.createTxOut `equals` slotNoParam)
            ( parenthesize $
                OrAExpr
                  (strictComparisonCond order (tableColumn @"txId" Schema.createTxOut) txIdParam)
                  ( parenthesize $
                      AndAExpr
                        (tableColumn @"txId" Schema.createTxOut `equals` txIdParam)
                        (laxComparisonCond order (tableColumn @"txIx" Schema.createTxOut) txIxParam)
                  )
            )
      )

-- | Adds additional checks to a condition as required by the contract filter.
filterCondition :: ContractFilter -> Maybe AExpr -> StatementBuilder (Maybe AExpr)
filterCondition ContractFilter{..} otherChecks = do
  tagExists <- tagExistsCond tags
  pure $
    otherChecks
      `andMaybe` tagExists
      `andMaybe` (addressPartyExists partyAddresses `orMaybe` rolePartyExists partyRoles)

andMaybe :: Maybe AExpr -> Maybe AExpr -> Maybe AExpr
andMaybe Nothing b = b
andMaybe a Nothing = a
andMaybe (Just a) (Just b) = Just $ parenthesize a `AndAExpr` parenthesize b

parenthesize :: (IsAExpr a) => a -> AExpr
parenthesize a = CExprAExpr $ InParensCExpr a Nothing

orMaybe :: Maybe AExpr -> Maybe AExpr -> Maybe AExpr
orMaybe Nothing b = b
orMaybe a Nothing = a
orMaybe (Just a) (Just b) = Just $ parenthesize a `OrAExpr` parenthesize b

tagExistsCond :: Set MarloweMetadataTag -> StatementBuilder (Maybe AExpr)
tagExistsCond tags
  | Set.null tags = pure Nothing
  | otherwise =
      Just <$> do
        tagsParam <- param $ Vector.fromList $ getMarloweMetadataTag <$> Set.toList tags
        let tagTable =
              unnestParams (pure tagsParam) $
                Just $
                  AliasFuncAliasClause $
                    AliasClause True "tags" $
                      Just $
                        pure "tag"

            fromClause = Schema.contractTxOutTag `naturalJoin` tagTable
            whereClause =
              AndAExpr
                (tableColumn @"txId" Schema.contractTxOutTag `equals` tableColumn @"txId" Schema.createTxOut)
                (tableColumn @"txIx" Schema.contractTxOutTag `equals` tableColumn @"txIx" Schema.createTxOut)
        pure $
          CExprAExpr $
            existsCond Nothing (Just $ pure fromClause) (Just whereClause) Nothing Nothing Nothing

rolePartyExists :: Set AssetId -> Maybe AExpr
rolePartyExists roles
  | Set.null roles = Nothing
  | otherwise = do
      let fromClause = Schema.contractTxOutPartyRole `naturalJoin` partyRolesTable
          whereClause =
            AndAExpr
              (tableColumn @"txId" Schema.createTxOut `equals` tableColumn @"createTxId" Schema.contractTxOutPartyRole)
              (tableColumn @"txIx" Schema.createTxOut `equals` tableColumn @"createTxIx" Schema.contractTxOutPartyRole)
      pure $ CExprAExpr $ existsCond Nothing (Just $ pure fromClause) (Just whereClause) Nothing Nothing Nothing

addressPartyExists :: Set Address -> Maybe AExpr
addressPartyExists addresses
  | Set.null addresses = Nothing
  | otherwise = do
      let fromClause = Schema.contractTxOutPartyAddress `naturalJoin` partyAddressesTable
          whereClause =
            AndAExpr
              (tableColumn @"txId" Schema.createTxOut `equals` tableColumn @"createTxId" Schema.contractTxOutPartyAddress)
              (tableColumn @"txIx" Schema.createTxOut `equals` tableColumn @"createTxIx" Schema.contractTxOutPartyAddress)
      pure $ CExprAExpr $ existsCond Nothing (Just $ pure fromClause) (Just whereClause) Nothing Nothing Nothing
