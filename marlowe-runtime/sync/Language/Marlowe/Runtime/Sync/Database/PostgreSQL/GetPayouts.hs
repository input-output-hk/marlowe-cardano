{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Language.Marlowe.Runtime.Sync.Database.PostgreSQL.GetPayouts where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT (..))
import Data.ByteString (ByteString)
import Data.Int (Int16, Int64)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Maybe (catMaybes)
import qualified Data.Set as Set
import Hasql.Decoders (Result)
import Hasql.Statement (Statement)
import qualified Hasql.Transaction as T
import Language.Marlowe.Protocol.Query.Types
import Language.Marlowe.Runtime.ChainSync.Api (
  AssetId (..),
  TxOutRef (..),
 )
import Language.Marlowe.Runtime.Core.Api (
  ContractId (..),
 )
import Language.Marlowe.Runtime.Sync.Database.PostgreSQL.SyntaxBuilder
import PostgresqlSyntax.Ast
import Prelude hiding (init)

-- | Fetch a page of payouts for a given filter and range.
getPayouts
  :: PayoutFilter
  -- ^ The filter, which controls which payouts are included in the result set.
  -> Range TxOutRef
  -- ^ The page range, which controls which results from the result set are returned, and in what order.
  -> T.Transaction (Maybe (Page TxOutRef PayoutRef))
getPayouts _ Range{..}
  -- Invalid requests. Note rangeLimit == 0 is invalid because it produces no
  -- results and causes infinite paging, which is potentially dangerous for
  -- clients as they could get caught in an infinite loop if consuming all
  -- pages.
  | rangeLimit <= 0 || rangeOffset < 0 = pure Nothing
getPayouts pFilter Range{rangeStart = Just payoutId, ..} = runMaybeT do
  -- In this transaction, the range states that the page should start from a specific payout. This payout, called the
  -- "delimiter," must exist in the result set. The transaction loads the delimiter in a separate query to confirm its\
  -- existence and saves it for later use.
  rangeDelimiter <- MaybeT $ T.statement () $ rangeDelimiterStatement pFilter payoutId
  -- Next, the transaction loads the size of the result set in a dedicated query to avoid unnecessarily loading the
  -- entire result set into memory.
  totalCount <- lift $ T.statement () $ totalCountStatement pFilter
  -- Finally, the transaction loads the requested page, referencing the previously loaded delimiter.
  lift $ T.statement () $ delimitedPayoutStatement pFilter totalCount rangeDelimiter rangeOffset rangeLimit rangeDirection
getPayouts pFilter range = do
  -- In this transaction, the page starts from the first or last payout, depending on the range direction. The
  -- transaction loads the size of the result set in a dedicated query to avoid unnecessarily loading the entire result
  -- set into memory.
  totalCount <- T.statement () $ totalCountStatement pFilter
  -- Next, the transaction loads the requested page.
  T.statement () $ Just <$> payoutStatement pFilter totalCount range

-- | An intermediate structure used as a reference for the start of a page.
data RangeDelimiter = RangeDelimiter
  { delimiterSlot :: Int64
  -- ^ The slot number of the block in which the delimiter payout appears.
  , delimiterTxId :: ByteString
  -- ^ TxId of the delimiter payout.
  , delimiterTxIx :: Int16
  -- ^ TxIx of the delimiter payout.
  }

-- | A Hasql statement that loads a the delimiter for a range which specifies a starting payout.
rangeDelimiterStatement
  :: PayoutFilter
  -- ^ The filter, which controls which payouts are included in the result set.
  -> TxOutRef
  -- ^ The identifier of the payout to use as the range delimiter
  -> Statement () (Maybe RangeDelimiter)
rangeDelimiterStatement pFilter _ = buildStatement rangeDelimiterPreparableStmt rangeDelimiterResult
  where
    -- Describes how to decode the result rows of the SELECT statement.
    rangeDelimiterResult :: Result (Maybe RangeDelimiter)
    rangeDelimiterResult = undefined

    -- The SELECT statement which will load the delimiter.
    rangeDelimiterPreparableStmt :: SyntaxBuilder PreparableStmt
    rangeDelimiterPreparableStmt =
      selectPreparableStmt $
        selectStmtNoParens $
          selectNoParens
            rangeDelimiterWithClause
            rangeDelimiterSelectClause
            Nothing
            Nothing
            Nothing

    rangeDelimiterWithClause :: Maybe (SyntaxBuilder WithClause)
    rangeDelimiterWithClause = withClause False <$> toNonEmpty (filterCTEs pFilter)

    rangeDelimiterSelectClause :: SyntaxBuilder SelectClause
    rangeDelimiterSelectClause =
      simpleSelect $
        normalSimpleSelect
          (Just rangeDelimiterTargeting)
          Nothing
          (Just rangeDelimiterFromClause)
          Nothing
          Nothing
          Nothing
          Nothing

    rangeDelimiterTargeting :: SyntaxBuilder Targeting
    rangeDelimiterTargeting =
      normalTargeting $
        NE.fromList
          [ column "applyTx" "slotNo"
          , column "payoutTxOut" "txId"
          , column "payoutTxOut" "txIx"
          ]

    rangeDelimiterFromClause :: NonEmpty (SyntaxBuilder TableRef)
    rangeDelimiterFromClause =
      pure $ payoutTxOutTableRef $ buildFromClause pFilter

buildFromClause :: PayoutFilter -> SyntaxBuilder TableRef -> SyntaxBuilder TableRef
buildFromClause PayoutFilter{..} payoutTxOut =
  joinTableRef (naturalJoin Nothing payoutTxOut $ applyTxTableRef $ addFilterJoins id) Nothing
  where
    addFilterJoins = addUnclaimedJoin . addContractIdsJoin . addRoleTokensJoin

    addContractIdsJoin f
      | Set.null contractIds = f
      | otherwise = \parent ->
          joinTableRef
            ( naturalJoin
                Nothing
                parent
                (contractIdsCTERef f)
            )
            Nothing

    addRoleTokensJoin f
      | Set.null roleTokens = f
      | otherwise = \parent ->
          joinTableRef
            ( naturalJoin
                Nothing
                parent
                (roleTokensCTERef f)
            )
            Nothing

    addUnclaimedJoin f
      | unclaimed = \parent ->
          joinTableRef
            ( methJoinedTable
                ( qualJoinMeth (Just $ LeftJoinType False) $
                    onJoinQual $
                      andAExpr
                        ( equalsAExpr
                            (cExprAExpr $ columnrefCExpr payoutTxOutTxIdCol)
                            (cExprAExpr $ columnrefCExpr withdrawalTxInPayoutTxIdCol)
                        )
                        ( equalsAExpr
                            (cExprAExpr $ columnrefCExpr payoutTxOutTxIxCol)
                            (cExprAExpr $ columnrefCExpr withdrawalTxInPayoutTxIxCol)
                        )
                )
                parent
                (withdrawalTxInRef f)
            )
            Nothing
      | otherwise = f

contractIdsCTERef :: (SyntaxBuilder TableRef -> SyntaxBuilder TableRef) -> SyntaxBuilder TableRef
contractIdsCTERef f = f $ table Nothing "contractIds"

roleTokensCTERef :: (SyntaxBuilder TableRef -> SyntaxBuilder TableRef) -> SyntaxBuilder TableRef
roleTokensCTERef f = f $ table Nothing "roleTokens"

applyTxTableRef :: (SyntaxBuilder TableRef -> SyntaxBuilder TableRef) -> SyntaxBuilder TableRef
applyTxTableRef f = f $ table (Just "marlowe") "applyTx"

payoutTxOutTableRef :: (SyntaxBuilder TableRef -> SyntaxBuilder TableRef) -> SyntaxBuilder TableRef
payoutTxOutTableRef f = f $ table (Just "marlowe") "payoutTxOut"

payoutTxOutTxIdCol :: Columnref
payoutTxOutTxIdCol = Columnref "payoutTxOut" $ Just $ pure $ AttrNameIndirectionEl "txId"

payoutTxOutTxIxCol :: Columnref
payoutTxOutTxIxCol = Columnref "payoutTxOut" $ Just $ pure $ AttrNameIndirectionEl "txIx"

withdrawalTxInRef :: (SyntaxBuilder TableRef -> SyntaxBuilder TableRef) -> SyntaxBuilder TableRef
withdrawalTxInRef f = f $ table (Just "marlowe") "withdrawalTxIn"

withdrawalTxInPayoutTxIdCol :: Columnref
withdrawalTxInPayoutTxIdCol = Columnref "withdrawalTxIn" $ Just $ pure $ AttrNameIndirectionEl "payoutTxId"

withdrawalTxInPayoutTxIxCol :: Columnref
withdrawalTxInPayoutTxIxCol = Columnref "withdrawalTxIn" $ Just $ pure $ AttrNameIndirectionEl "payoutTxIx"

toNonEmpty :: [a] -> Maybe (NonEmpty a)
toNonEmpty [] = Nothing
toNonEmpty (x : xs) = Just $ x :| xs

filterCTEs :: PayoutFilter -> [SyntaxBuilder CommonTableExpr]
filterCTEs pFilter = catMaybes [contractIdsCTE pFilter, roleTokensCTE pFilter]

contractIdsCTE :: PayoutFilter -> Maybe (SyntaxBuilder CommonTableExpr)
contractIdsCTE PayoutFilter{..}
  | Set.null contractIds = Nothing
  | otherwise =
      Just
        $ commonTableExpr
          "contractIds"
          (Just $ NE.fromList ["createTxId", "createTxIx"])
          Nothing
        $ selectPreparableStmt
        $ selectStmtNoParens
        $ selectNoParens Nothing selectClause Nothing Nothing Nothing
  where
    selectClause = simpleSelect $ normalSimpleSelect (Just targeting) Nothing (Just fromClause) Nothing Nothing Nothing Nothing
    targeting = normalTargeting $ pure asteriskTargetEl
    fromClause =
      pure $
        funcTableRef
          False
          ( funcExprFuncTable
              ( applicationFuncExprWindowless $
                  simpleFuncApplication
                    "unnest"
                    [ exprFuncArgExpr $ cExprAExpr $ paramCExpr $ Set.map (txId . unContractId) contractIds
                    , exprFuncArgExpr $ cExprAExpr $ paramCExpr $ Set.map (txIx . unContractId) contractIds
                    ]
              )
              False
          )
          Nothing

roleTokensCTE :: PayoutFilter -> Maybe (SyntaxBuilder CommonTableExpr)
roleTokensCTE PayoutFilter{..}
  | Set.null roleTokens = Nothing
  | otherwise =
      Just
        $ commonTableExpr
          "roleTokens"
          (Just $ NE.fromList ["rolesCurrency", "role"])
          Nothing
        $ selectPreparableStmt
        $ selectStmtNoParens
        $ selectNoParens Nothing selectClause Nothing Nothing Nothing
  where
    selectClause = simpleSelect $ normalSimpleSelect (Just targeting) Nothing (Just fromClause) Nothing Nothing Nothing Nothing
    targeting = normalTargeting $ pure asteriskTargetEl
    fromClause =
      pure $
        funcTableRef
          False
          ( funcExprFuncTable
              ( applicationFuncExprWindowless $
                  simpleFuncApplication
                    "unnest"
                    [ exprFuncArgExpr $ cExprAExpr $ paramCExpr $ Set.map policyId roleTokens
                    , exprFuncArgExpr $ cExprAExpr $ paramCExpr $ Set.map tokenName roleTokens
                    ]
              )
              False
          )
          Nothing

totalCountStatement :: PayoutFilter -> Statement () Int
totalCountStatement = undefined

payoutStatement :: PayoutFilter -> Int -> Range TxOutRef -> Statement () (Page TxOutRef PayoutRef)
payoutStatement = undefined

delimitedPayoutStatement :: PayoutFilter -> Int -> t0 -> Int -> Int -> Order -> Statement () (Page TxOutRef PayoutRef)
delimitedPayoutStatement = undefined
