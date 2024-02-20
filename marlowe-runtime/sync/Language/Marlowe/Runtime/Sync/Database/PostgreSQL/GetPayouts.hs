{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Language.Marlowe.Runtime.Sync.Database.PostgreSQL.GetPayouts where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT (..))
import Data.ByteString (ByteString)
import Data.Int (Int16, Int32, Int64)
import qualified Data.List.NonEmpty as NE
import Data.Maybe (catMaybes, isJust)
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Vector as Vector
import qualified Hasql.Decoders as Decoders
import Hasql.DynamicSyntax.Ast
import Hasql.DynamicSyntax.Schema
import Hasql.DynamicSyntax.Statement (StatementBuilder, buildStatement, param)
import Hasql.Statement (Statement)
import qualified Hasql.Transaction as T
import Language.Marlowe.Protocol.Query.Types (
  Order (..),
  Page (..),
  PayoutFilter (..),
  PayoutHeader (..),
  Range (..),
 )
import Language.Marlowe.Runtime.ChainSync.Api (
  AssetId (..),
  PolicyId (..),
  TokenName (..),
  TxId (..),
  TxOutRef (..),
  unTxIx,
 )
import Language.Marlowe.Runtime.Core.Api (ContractId (..))
import Language.Marlowe.Runtime.Schema (countAll, equals, innerJoinOn, leftJoinOn, naturalJoin, unnestParams, withCTEs)
import qualified Language.Marlowe.Runtime.Schema as Schema
import Language.Marlowe.Runtime.Sync.Database.PostgreSQL.GetWithdrawal (decodePayoutHeader)
import PostgresqlSyntax.Ast (
  AscDesc (..),
  MathOp (..),
  SymbolicExprBinOp (..),
 )
import Prelude hiding (init)

-- | Fetch a page of payouts for a given filter and range.
getPayouts
  :: PayoutFilter
  -- ^ The filter, which controls which payouts are included in the result set.
  -> Range TxOutRef
  -- ^ The page range, which controls which results from the result set are returned, and in what order.
  -> T.Transaction (Maybe (Page TxOutRef PayoutHeader))
getPayouts _ Range{..}
  -- Invalid requests. Note rangeLimit == 0 is invalid because it produces no
  -- results and causes infinite paging, which is potentially dangerous for
  -- clients as they could get caught in an infinite loop if consuming all
  -- pages.
  | rangeLimit <= 0 || rangeOffset < 0 = pure Nothing
getPayouts pFilter range@Range{..} = runMaybeT do
  mDelimiter <- traverse (MaybeT . T.statement () . delimiterStatement pFilter) rangeStart
  lift do
    totalCount <- T.statement () $ totalCountStatement pFilter
    itemsWithNext <- T.statement () $ payoutsStatement pFilter range mDelimiter
    let nextRange = case drop rangeLimit itemsWithNext of
          [] -> Nothing
          PayoutHeader{..} : _ -> Just Range{rangeStart = Just payoutId, rangeOffset = 0, ..}
        items = take rangeLimit itemsWithNext
    pure Page{..}

-- * Statements

data DelimiterRow = DelimiterRow
  { delimiterTxId :: ByteString
  , delimiterTxIx :: Int16
  , delimiterSlotNo :: Int64
  }

-- | A select statement which looks for the delimiter specified by the given payoutId.
delimiterStatement :: PayoutFilter -> TxOutRef -> Statement () (Maybe DelimiterRow)
delimiterStatement pFilter TxOutRef{..} = buildStatement DelimiterRow Decoders.rowMaybe do
  -- Allocate the tables and the CTEs.
  (withClause, fromClause) <- tables False pFilter
  -- Allocate parameters for the delimiter.
  txIdParam <- param $ unTxId txId
  txIxParam <- param @Int16 $ fromIntegral $ unTxIx txIx
  let selectClause =
        NormalSimpleSelect
          ( NormalTargeting $
              tableColumn @"txId" Schema.payoutTxOut
                :. tableColumn @"txIx" Schema.payoutTxOut
                :. tableColumn @"slotNo" Schema.applyTx
                :. TargetListNil
          )
          Nothing
          (Just $ pure fromClause)
          (filterCondition pFilter $ Just $ delimiterIdCond txIdParam txIxParam)
          Nothing
          Nothing
          Nothing
  pure $ SelectPreparableStmt $ Left $ SelectNoParens withClause (Left selectClause) Nothing Nothing Nothing

-- | A select statement which counts the total number of results matching the filter.
totalCountStatement :: PayoutFilter -> Statement () Int
totalCountStatement pFilter = buildStatement fromIntegral Decoders.singleRow do
  -- Allocate the tables and the CTEs.
  (withClause, fromClause) <- tables False pFilter
  let selectClause =
        NormalSimpleSelect
          (NormalTargeting $ countAll :. TargetListNil)
          Nothing
          (Just $ pure fromClause)
          (filterCondition pFilter $ Just $ toAExpr True)
          Nothing
          Nothing
          Nothing
  pure $ SelectPreparableStmt $ Left $ SelectNoParens withClause (Left selectClause) Nothing Nothing Nothing

-- | A select statement which fetches the page of payouts.
payoutsStatement :: PayoutFilter -> Range TxOutRef -> Maybe DelimiterRow -> Statement () [PayoutHeader]
payoutsStatement pFilter Range{..} mDelimiter = buildStatement decodePayoutHeader Decoders.rowList do
  -- Allocate the tables and the CTEs.
  (withClause, fromClause) <- tables True pFilter
  -- Allocate the where clause
  whereClause <- filterCondition pFilter <$> traverse (delimiterComparisonCond rangeDirection) mDelimiter
  -- Allocate limit and offset params
  offsetParam <- param $ fromIntegral @_ @Int32 rangeOffset
  limitParam <- param $ fromIntegral @_ @Int32 (rangeLimit + 1)
  let selectClause =
        NormalSimpleSelect
          ( NormalTargeting $
              tableColumn @"txId" withdrawalTxInTable
                :. tableColumn @"createTxId" Schema.applyTx
                :. tableColumn @"createTxIx" Schema.applyTx
                :. tableColumn @"txId" Schema.payoutTxOut
                :. tableColumn @"txIx" Schema.payoutTxOut
                :. tableColumn @"rolesCurrency" Schema.payoutTxOut
                :. tableColumn @"role" Schema.payoutTxOut
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
          [ rangeSortBy rangeDirection $ tableColumn @"slotNo" Schema.applyTx
          , rangeSortBy rangeDirection $ tableColumn @"txId" Schema.payoutTxOut
          , rangeSortBy rangeDirection $ tableColumn @"txIx" Schema.payoutTxOut
          ]
      selectLimit =
        OffsetLimitSelectLimit offsetParam $
          FetchOnlyLimitClause True (Just $ toSelectFetchFirstValue limitParam) True
  pure $
    SelectPreparableStmt $
      Left $
        SelectNoParens withClause (Left selectClause) (Just sortClause) (Just selectLimit) Nothing

rangeSortBy :: (IsAExpr a) => Order -> a -> SortBy
rangeSortBy Descending a = AscDescSortBy a (Just DescAscDesc) Nothing
rangeSortBy Ascending a = AscDescSortBy a Nothing Nothing

-- * Tables

-- | A modified withdrawal tx in table with all columns null, for left joins.
withdrawalTxInTable :: Table (AllNull Schema.WithdrawalTxInColumns)
withdrawalTxInTable = allNull Schema.withdrawalTxIn

-- | The columns of the contract IDs CTE
type ContractIdsColumns =
  '[ '("createTxId", SqlBytea, NotNull)
   , '("createTxIx", SqlInt2, NotNull)
   ]

-- | The contract IDs CTE table
contractIdsTable :: Table ContractIdsColumns
contractIdsTable = Schema.tempTable "contractIds"

-- |
--  ==== SQL
--  @
--  contractIds (createTxId, createTxIx)
--     ( SELECT * FROM UNNEST ($1, $2)
--     )
--  @
contractIdsCTE :: Param -> Param -> CommonTableExpr
contractIdsCTE idsParam ixsParam =
  cte contractIdsTable . simpleSelectPreparableStmt $
    NormalSimpleSelect
      (wildcard contractIdsTable)
      Nothing
      (Just $ pure $ unnestParams (NE.fromList [idsParam, ixsParam]) Nothing)
      Nothing
      Nothing
      Nothing
      Nothing

-- | The columns of the role tokens CTE
type RoleTokensColumns =
  '[ '("rolesCurrency", SqlBytea, NotNull)
   , '("role", SqlBytea, NotNull)
   ]

-- | The role tokens CTE table
roleTokensTable :: Table RoleTokensColumns
roleTokensTable = Schema.tempTable "roleTokens"

-- |
--  ==== SQL
--  @
--  roleTokens (rolesCurrency, role)
--     ( SELECT * FROM UNNEST ($1, $2)
--     )
--  @
roleTokensCTE :: Param -> Param -> CommonTableExpr
roleTokensCTE policiesParam namesParam =
  cte roleTokensTable . simpleSelectPreparableStmt $
    NormalSimpleSelect
      (wildcard roleTokensTable)
      Nothing
      (Just $ pure $ unnestParams (NE.fromList [policiesParam, namesParam]) Nothing)
      Nothing
      Nothing
      Nothing
      Nothing

-- | Compiles the CTEs and FROM clause table ref for a query given a filter.
tables :: Bool -> PayoutFilter -> StatementBuilder (Maybe WithClause, TableRef)
tables alwaysJoinWithdrawals PayoutFilter{..} = do
  (mContractIdsCTE, joinContractIds) <- contractIdsTables contractIds
  (mRoleTokensCTE, joinRoleTokens) <- roleTokensTables roleTokens
  pure
    ( withCTEs False $ catMaybes [mContractIdsCTE, mRoleTokensCTE]
    , baseTables alwaysJoinWithdrawals PayoutFilter{..} (joinRoleTokens . joinContractIds)
    )

-- | The joined tables from the marlowe schema used in the FROM clause for a given filter.
baseTables :: Bool -> PayoutFilter -> (TableRef -> TableRef) -> TableRef
baseTables alwaysJoinWithdrawals PayoutFilter{..} insertCTEJoins
  | isWithdrawn == Just True = innerJoinOn payoutWithdrawalJoinCond tablesWithoutWithdrawals withdrawalTxInTable
  | shouldJoinWithdrawals = leftJoinOn payoutWithdrawalJoinCond tablesWithoutWithdrawals withdrawalTxInTable
  | otherwise = tablesWithoutWithdrawals
  where
    tablesWithoutWithdrawals = insertCTEJoins $ Schema.payoutTxOut `naturalJoin` Schema.applyTx
    shouldJoinWithdrawals = alwaysJoinWithdrawals || isJust isWithdrawn

-- | The CTE and a modification to the FROM clause for the filtered contract IDs.
contractIdsTables :: Set ContractId -> StatementBuilder (Maybe CommonTableExpr, TableRef -> TableRef)
contractIdsTables contractIds
  -- If none are specified, skip adding the CTE and pass the FROM clause through unchanged.
  | Set.null contractIds = pure (Nothing, id)
  | otherwise = do
      -- Process the set of ContractIds into two vectors of ByteStrings and Int16s to be used as parameters
      let contractIdsVector = unContractId <$> Vector.fromList (Set.toList contractIds)
      let txIds = unTxId . txId <$> contractIdsVector
      let txIxs = fromIntegral @_ @Int16 . unTxIx . txIx <$> contractIdsVector
      -- Allocate the parameters
      txIdsParam <- param txIds
      txIxsParam <- param txIxs
      pure
        -- Add the CTE to the select statement
        ( Just $ contractIdsCTE txIdsParam txIxsParam
        , -- Modify the FROM clause by joining the contractIds CTE to it.
          flip naturalJoin contractIdsTable
        )

-- | The CTE and a modification to the FROM clause for the filtered role tokens.
roleTokensTables :: Set AssetId -> StatementBuilder (Maybe CommonTableExpr, TableRef -> TableRef)
roleTokensTables roleTokens
  -- If none are specified, skip adding the CTE and pass the FROM clause through unchanged.
  | Set.null roleTokens = pure (Nothing, id)
  | otherwise = do
      -- Process the set of role tokens into two vectors of ByteStrings to be used as parameters
      let roleTokensVector = Vector.fromList (Set.toList roleTokens)
      let policyIds = unPolicyId . policyId <$> roleTokensVector
      let tokenNames = unTokenName . tokenName <$> roleTokensVector
      -- Allocate the parameters
      policyIdsParam <- param policyIds
      tokenNamesParam <- param tokenNames
      pure
        -- Add the CTE to the select statement
        ( Just $ roleTokensCTE policyIdsParam tokenNamesParam
        , -- Modify the FROM clause by joining the roleTokens CTE to it.
          flip naturalJoin roleTokensTable
        )

-- * conditions

-- |
--  ==== SQL
--  @
--  payoutTxOut.txId = withdrawalTxIn.payoutTxId
--    AND payoutTxOut.txIx = withdrawalTxIn.payoutTxIx
--  @
payoutWithdrawalJoinCond :: AExpr
payoutWithdrawalJoinCond =
  AndAExpr
    (tableColumn @"txId" Schema.payoutTxOut `equals` tableColumn @"payoutTxId" withdrawalTxInTable)
    (tableColumn @"txIx" Schema.payoutTxOut `equals` tableColumn @"payoutTxIx" withdrawalTxInTable)

-- |
--  ==== SQL
--  @
--  payoutTxOut.txId = $1 AND payoutTxOut.txIx = $2
--  @
delimiterIdCond :: Param -> Param -> AExpr
delimiterIdCond txIdParam txIxParam =
  AndAExpr
    (tableColumn @"txId" Schema.payoutTxOut `equals` txIdParam)
    (tableColumn @"txIx" Schema.payoutTxOut `equals` txIxParam)

delimiterComparisonCond :: Order -> DelimiterRow -> StatementBuilder AExpr
delimiterComparisonCond order DelimiterRow{..} = do
  slotNoParam <- param delimiterSlotNo
  txIdParam <- param delimiterTxId
  txIxParam <- param delimiterTxIx
  pure $
    OrAExpr
      (strictComparisonCond order (tableColumn @"slotNo" Schema.applyTx) slotNoParam)
      ( flip InParensCExpr Nothing $
          AndAExpr
            (tableColumn @"slotNo" Schema.applyTx `equals` slotNoParam)
            ( flip InParensCExpr Nothing $
                OrAExpr
                  (strictComparisonCond order (tableColumn @"txId" Schema.payoutTxOut) txIdParam)
                  ( flip InParensCExpr Nothing $
                      AndAExpr
                        (tableColumn @"txId" Schema.payoutTxOut `equals` txIdParam)
                        (laxComparisonCond order (tableColumn @"txIx" Schema.payoutTxOut) txIxParam)
                  )
            )
      )

strictComparisonCond :: (IsAExpr a, IsAExpr b) => Order -> a -> b -> AExpr
strictComparisonCond Descending a = SymbolicBinOpAExpr a (MathSymbolicExprBinOp ArrowLeftMathOp)
strictComparisonCond Ascending a = SymbolicBinOpAExpr a (MathSymbolicExprBinOp ArrowRightMathOp)

laxComparisonCond :: (IsAExpr a, IsAExpr b) => Order -> a -> b -> AExpr
laxComparisonCond Descending a = SymbolicBinOpAExpr a (MathSymbolicExprBinOp LessEqualsMathOp)
laxComparisonCond Ascending a = SymbolicBinOpAExpr a (MathSymbolicExprBinOp GreaterEqualsMathOp)

-- | Adds additional checks to a condition as required by the payout filter.
filterCondition :: PayoutFilter -> Maybe AExpr -> Maybe AExpr
filterCondition PayoutFilter{..} = case isWithdrawn of
  Nothing -> id
  Just False -> Just . maybe payoutNotWithdrawnCond ((`AndAExpr` payoutNotWithdrawnCond) . flip InParensCExpr Nothing)
  Just True -> Just . maybe payoutIsWithdrawnCond ((`AndAExpr` payoutIsWithdrawnCond) . flip InParensCExpr Nothing)

-- |
--  ==== SQL
--  @
--  withdrawalTxIn.txId NOTNULL
--  @
payoutIsWithdrawnCond :: AExpr
payoutIsWithdrawnCond = NotnullAExpr $ tableColumn @"txId" withdrawalTxInTable

-- |
--  ==== SQL
--  @
--  withdrawalTxIn.txId ISNULL
--  @
payoutNotWithdrawnCond :: AExpr
payoutNotWithdrawnCond = IsnullAExpr $ tableColumn @"txId" withdrawalTxInTable
