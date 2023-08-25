{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE QuasiQuotes #-}

module Language.Marlowe.Runtime.Sync.Database.PostgreSQL.GetPayouts where

import Control.Monad (guard)
import Data.ByteString (ByteString)
import Data.Int (Int16, Int64)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Maybe (catMaybes, isJust, listToMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Vector as V
import qualified Data.Vector as Vector
import Hasql.DynamicSyntax.Ast
import Hasql.DynamicSyntax.Schema
import Hasql.DynamicSyntax.Statement (StatementBuilder, param)
import Hasql.TH (vectorStatement)
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
 )
import Language.Marlowe.Runtime.Core.Api (ContractId (..))
import Language.Marlowe.Runtime.Schema (equals, leftJoinOn, naturalJoin, withCTEs)
import qualified Language.Marlowe.Runtime.Schema as Schema
import Language.Marlowe.Runtime.Sync.Database.PostgreSQL.GetWithdrawal (decodePayoutHeader)
import Prelude hiding (init)

-- | Fetch a page of payouts for a given filter and range.
getPayouts
  :: PayoutFilter
  -- ^ The filter, which controls which payouts are included in the result set.
  -> Range TxOutRef
  -- ^ The page range, which controls which results from the result set are returned, and in what order.
  -> T.Transaction (Maybe (Page TxOutRef PayoutHeader))
getPayouts PayoutFilter{..} Range{..} = do
  -- FIXME this is a temporary, limited and memory-intensive implementation that needs to be replaced with dynamic SQL.
  allPayouts <-
    V.toList . fmap (uncurry7 decodePayoutHeader)
      <$> T.statement
        ()
        case isWithdrawn of
          Just True ->
            [vectorStatement|
              SELECT
                withdrawalTxIn.txId :: bytea?,
                applyTx.createTxId :: bytea,
                applyTx.createTxIx :: smallint,
                payoutTxOut.txId :: bytea,
                payoutTxOut.txIx :: smallint,
                payoutTxOut.rolesCurrency :: bytea,
                payoutTxOut.role :: bytea
              FROM marlowe.payoutTxOut
              NATURAL JOIN marlowe.applyTx
              LEFT JOIN marlowe.withdrawalTxIn
                ON payoutTxOut.txId = withdrawalTxIn.payoutTxId
                AND payoutTxOut.txIx = withdrawalTxIn.payoutTxIx
              WHERE withdrawalTxIn.txId IS NOT NULL
              ORDER BY applyTx.slotNo, payoutTxOut.txId, payoutTxOut.txIx
            |]
          Just False ->
            [vectorStatement|
              SELECT
                withdrawalTxIn.txId :: bytea?,
                applyTx.createTxId :: bytea,
                applyTx.createTxIx :: smallint,
                payoutTxOut.txId :: bytea,
                payoutTxOut.txIx :: smallint,
                payoutTxOut.rolesCurrency :: bytea,
                payoutTxOut.role :: bytea
              FROM marlowe.payoutTxOut
              NATURAL JOIN marlowe.applyTx
              LEFT JOIN marlowe.withdrawalTxIn
                ON payoutTxOut.txId = withdrawalTxIn.payoutTxId
                AND payoutTxOut.txIx = withdrawalTxIn.payoutTxIx
              WHERE withdrawalTxIn.txId IS NULL
              ORDER BY applyTx.slotNo, payoutTxOut.txId, payoutTxOut.txIx
            |]
          Nothing ->
            [vectorStatement|
              SELECT
                withdrawalTxIn.txId :: bytea?,
                applyTx.createTxId :: bytea,
                applyTx.createTxIx :: smallint,
                payoutTxOut.txId :: bytea,
                payoutTxOut.txIx :: smallint,
                payoutTxOut.rolesCurrency :: bytea,
                payoutTxOut.role :: bytea
              FROM marlowe.payoutTxOut
              NATURAL JOIN marlowe.applyTx
              LEFT JOIN marlowe.withdrawalTxIn
                ON payoutTxOut.txId = withdrawalTxIn.payoutTxId
                AND payoutTxOut.txIx = withdrawalTxIn.payoutTxIx
              ORDER BY applyTx.slotNo, payoutTxOut.txId, payoutTxOut.txIx
            |]
  pure do
    let contractIdsFiltered
          | Set.null contractIds = allPayouts
          | otherwise = filter (flip Set.member contractIds . contractId) allPayouts
    let filtered
          | Set.null roleTokens = contractIdsFiltered
          | otherwise = filter (flip Set.member roleTokens . role) contractIdsFiltered
    let ordered = case rangeDirection of
          Ascending -> filtered
          Descending -> reverse filtered
    delimited <- case rangeStart of
      Nothing -> pure ordered
      Just startFrom -> do
        guard $ any ((== startFrom) . payoutId) ordered
        pure $ dropWhile ((/= startFrom) . payoutId) ordered
    let items = take rangeLimit . drop rangeOffset $ delimited
    pure
      Page
        { items
        , nextRange = do
            PayoutHeader{..} <- listToMaybe $ reverse items
            pure $ Range{rangeStart = Just payoutId, rangeOffset = 1, ..}
        , totalCount = length filtered
        }

-- * Statements

data Delimiter = Delimiter
  { delimiterTxId :: ByteString
  , delimiterTxIx :: Int16
  , delimiterSlotNo :: Int64
  }

type DelimiterRow = '[ '[ '(SqlBytea, NotNull)], '[ '(SqlInt2, NotNull)], '[ '(SqlInt8, NotNull)]]

-- | A select statement which looks for the delimiter specified by the given payoutId.
delimiterStatement :: PayoutFilter -> TxOutRef -> StatementBuilder (PreparableStmt DelimiterRow)
delimiterStatement pFilter TxOutRef{..} = do
  -- Allocate the tables and the CTEs.
  (withClause, fromClause) <- tables False pFilter
  -- Allocate parameters for the delimiter.
  txIdParam <- param $ unTxId txId
  txIxParam <- param @Int16 $ fromIntegral txIx
  let selectClause =
        Left $
          NormalSimpleSelect
            ( NormalTargeting $
                tableColumn @"txId" Schema.payoutTxOut
                  :. tableColumn @"txIx" Schema.payoutTxOut
                  :. tableColumn @"slotNo" Schema.applyTx
                  :. TargetListNil
            )
            Nothing
            (Just $ pure fromClause)
            (Just $ filterCondition pFilter $ delimiterIdCond txIdParam txIxParam)
            Nothing
            Nothing
            Nothing
  pure $ SelectPreparableStmt $ Left $ SelectNoParens withClause selectClause Nothing Nothing Nothing

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
      (Just $ pure $ unnestParams $ NE.fromList [idsParam, ixsParam])
      Nothing
      Nothing
      Nothing
      Nothing

-- | The columns of the role tokens CTE
type RoleTokensColumns =
  '[ '("rolesCurrency", SqlBytea, NotNull)
   , '("\"role\"", SqlBytea, NotNull)
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
      (Just $ pure $ unnestParams $ NE.fromList [policiesParam, namesParam])
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
    , joinRoleTokens $ joinContractIds $ baseTables alwaysJoinWithdrawals PayoutFilter{..}
    )

-- | The joined tables from the marlowe schema used in the FROM clause for a given filter.
baseTables :: Bool -> PayoutFilter -> TableRef
baseTables alwaysJoinWithdrawals PayoutFilter{..}
  | shouldJoinWithdrawals =
      naturalJoin Schema.payoutTxOut $
        leftJoinOn payoutWithdrawalJoinCond Schema.applyTx withdrawalTxInTable
  | otherwise = Schema.payoutTxOut `naturalJoin` Schema.applyTx
  where
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
      let txIxs = fromIntegral @_ @Int16 . txIx <$> contractIdsVector
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

-- | Adds additional checks to a condition as required by the payout filter.
filterCondition :: PayoutFilter -> AExpr -> AExpr
filterCondition PayoutFilter{..} = case isWithdrawn of
  Nothing -> id
  Just False -> (`AndAExpr` payoutNotWithdrawnCond)
  Just True -> (`AndAExpr` payoutIsWithdrawnCond)

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

uncurry7 :: (a -> b -> c -> d -> e -> f -> g -> h) -> (a, b, c, d, e, f, g) -> h
uncurry7 f' (a, b, c, d, e, f, g) = f' a b c d e f g

unnestParams :: NonEmpty Param -> TableRef
unnestParams params =
  FuncTableRef
    False
    ( FuncExprFuncTable
        ( ApplicationFuncExprWindowless $
            FuncApplication "UNNEST" $
              Just $
                NormalFuncApplicationParams Nothing (paramFuncArgExpr <$> params) Nothing
        )
        False
    )
    Nothing
