{-# LANGUAGE DataKinds #-}

module Language.Marlowe.Runtime.Sync.Database.PostgreSQL.GetRoleCurrencies (
  getRoleCurrencies,
)
where

import Control.Arrow (Arrow (..))
import Control.Monad.Trans.Class (MonadTrans (..))
import Control.Monad.Trans.RWS (RWST (..), state, tell)
import Data.ByteString (ByteString)
import Data.Int (Int16)
import qualified Data.List.NonEmpty as NE
import Data.Maybe (isNothing)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.String (IsString (..))
import qualified Data.Vector as V
import qualified Hasql.Decoders as Decoders
import Hasql.DynamicSyntax.Ast (
  AExpr (AndAExpr, IsnullAExpr, NotAExpr, OrAExpr),
  CExpr (ExistsCExpr),
  CommonTableExpr,
  IsAExpr (toAExpr),
  IsTableRef (..),
  NotNull,
  PreparableStmt (SelectPreparableStmt),
  SelectNoParens (SelectNoParens),
  SelectWithParens (NoParensSelectWithParens),
  SimpleSelect (NormalSimpleSelect),
  SortBy (AscDescSortBy),
  SqlBytea,
  SqlInt2,
  TableRef,
  TargetList (TargetListNil, (:.)),
  Targeting (NormalTargeting),
  simpleSelectPreparableStmt,
 )
import Hasql.DynamicSyntax.Schema (allNull, cte, tableColumn, wildcard)
import Hasql.DynamicSyntax.Statement (
  StatementBuilder,
  buildStatement,
  param,
 )
import qualified Hasql.Transaction as HT
import Language.Marlowe.Protocol.Query.Types (
  RoleCurrency (..),
  RoleCurrencyFilter (..),
 )
import Language.Marlowe.Runtime.ChainSync.Api (PolicyId (PolicyId, unPolicyId), TxId (..), TxIx (..), TxOutRef (..))
import Language.Marlowe.Runtime.Core.Api (ContractId (..))
import Language.Marlowe.Runtime.Schema (equals, leftJoinOn, naturalJoin, unnestParams, withCTEs)
import qualified Language.Marlowe.Runtime.Schema as Schema
import Language.Marlowe.Runtime.Sync.Database.PostgreSQL.GetContractState (decodeContractId)
import Language.Marlowe.Runtime.Sync.Database.PostgreSQL.GetHeaders (parenthesize)
import Numeric.Natural (Natural)

getRoleCurrencies :: RoleCurrencyFilter -> HT.Transaction (Set RoleCurrency)
getRoleCurrencies RoleCurrencyFilterNone = pure mempty
getRoleCurrencies cFilter =
  Set.fromDistinctAscList <$> HT.statement () stmt
  where
    stmt = buildStatement decodeResultRow Decoders.rowList do
      (whereClause, _, ctes) <- runRWST (compileFilter cFilter) () (0, 0)
      let withClause = withCTEs False ctes
      let selectClause =
            NormalSimpleSelect
              ( NormalTargeting $
                  tableColumn @"rolesCurrency" Schema.contractTxOut
                    :. tableColumn @"txId" Schema.createTxOut
                    :. tableColumn @"txIx" Schema.createTxOut
                    :. tableColumn @"txId" (allNull Schema.applyTx)
                    :. TargetListNil
              )
              Nothing
              ( Just $
                  pure $
                    Schema.createTxOut
                      `naturalJoin` Schema.contractTxOut
                      `leftJoinOnClose` Schema.applyTx
              )
              (Just whereClause)
              Nothing
              Nothing
              Nothing
      let sortClause =
            NE.fromList
              [ AscDescSortBy (tableColumn @"rolesCurrency" Schema.contractTxOut) Nothing Nothing
              , AscDescSortBy (tableColumn @"txId" Schema.createTxOut) Nothing Nothing
              , AscDescSortBy (tableColumn @"txIx" Schema.createTxOut) Nothing Nothing
              ]
      pure $ SelectPreparableStmt $ Left $ SelectNoParens withClause (Left selectClause) (Just sortClause) Nothing Nothing

leftJoinOnClose :: (IsTableRef a, IsTableRef b) => a -> b -> TableRef
leftJoinOnClose = leftJoinOn expr
  where
    expr =
      (tableColumn @"txId" Schema.createTxOut `equals` tableColumn @"createTxId" Schema.applyTx)
        `AndAExpr` (tableColumn @"txIx" Schema.createTxOut `equals` tableColumn @"createTxIx" Schema.applyTx)
        `AndAExpr` (IsnullAExpr $ tableColumn @"outputTxIx" Schema.applyTx)

type ContractsColumns =
  '[ '("txId", SqlBytea, NotNull)
   , '("txIx", SqlInt2, NotNull)
   ]

type PoliciesColumns =
  '[ '("rolesCurrency", SqlBytea, NotNull)
   ]

compileFilter :: RoleCurrencyFilter -> RWST () [CommonTableExpr] (Natural, Natural) StatementBuilder AExpr
compileFilter = \case
  RoleCurrencyAnd a b -> do
    a' <- compileFilter a
    b' <- compileFilter b
    pure $ parenthesize a' `AndAExpr` parenthesize b'
  RoleCurrencyOr a b -> do
    a' <- compileFilter a
    b' <- compileFilter b
    pure $ parenthesize a' `OrAExpr` parenthesize b'
  RoleCurrencyNot a -> NotAExpr . parenthesize <$> compileFilter a
  RoleCurrencyFilterNone -> pure $ toAExpr False
  RoleCurrencyFilterAny -> pure $ toAExpr True
  RoleCurrencyFilterByContract contracts -> do
    contractTableNumber <- state \(c, p) -> (c, (succ c, p))
    let contracts' = V.fromList $ Set.toList $ Set.map ((txId &&& txIx) . unContractId) contracts
    contractTxIdsParam <- lift $ param $ unTxId . fst <$> contracts'
    contractTxIxsParam <- lift $ param $ (\(TxIx a) -> fromIntegral @_ @Int16 a) . snd <$> contracts'
    let contractsTable = Schema.tempTable @ContractsColumns $ fromString $ "contracts" <> show contractTableNumber
    let contractsCte =
          cte contractsTable . simpleSelectPreparableStmt $
            NormalSimpleSelect
              (wildcard contractsTable)
              Nothing
              (Just $ pure $ unnestParams (NE.fromList [contractTxIdsParam, contractTxIxsParam]) Nothing)
              Nothing
              Nothing
              Nothing
              Nothing
    let selectClause =
          NormalSimpleSelect
            (wildcard contractsTable)
            Nothing
            (Just $ pure $ toTableRef contractsTable)
            ( Just $
                AndAExpr
                  (tableColumn @"txId" Schema.createTxOut `equals` tableColumn @"txId" contractsTable)
                  (tableColumn @"txIx" Schema.createTxOut `equals` tableColumn @"txIx" contractsTable)
            )
            Nothing
            Nothing
            Nothing
    tell [contractsCte]
    pure $
      toAExpr $
        ExistsCExpr $
          NoParensSelectWithParens $
            SelectNoParens
              Nothing
              (Left selectClause)
              Nothing
              Nothing
              Nothing
  RoleCurrencyFilterByPolicy policies -> do
    policyTableNumber <- state \(c, p) -> (p, (c, succ p))
    policiesParam <- lift $ param $ V.fromList $ unPolicyId <$> Set.toList policies
    let policiesTable = Schema.tempTable @PoliciesColumns $ fromString $ "policies" <> show policyTableNumber
    let policiesCte =
          cte policiesTable . simpleSelectPreparableStmt $
            NormalSimpleSelect
              (wildcard policiesTable)
              Nothing
              (Just $ pure $ unnestParams (pure policiesParam) Nothing)
              Nothing
              Nothing
              Nothing
              Nothing
    let selectClause =
          NormalSimpleSelect
            (wildcard policiesTable)
            Nothing
            (Just $ pure $ toTableRef policiesTable)
            (Just $ tableColumn @"rolesCurrency" Schema.contractTxOut `equals` tableColumn @"rolesCurrency" policiesTable)
            Nothing
            Nothing
            Nothing
    tell [policiesCte]
    pure $
      toAExpr $
        ExistsCExpr $
          NoParensSelectWithParens $
            SelectNoParens
              Nothing
              (Left selectClause)
              Nothing
              Nothing
              Nothing

decodeResultRow :: ByteString -> ByteString -> Int16 -> Maybe ByteString -> RoleCurrency
decodeResultRow policyId txId txIx closeTx =
  RoleCurrency
    { rolePolicyId = PolicyId policyId
    , roleContract = decodeContractId txId txIx
    , active = isNothing closeTx
    }
