{-# LANGUAGE DataKinds #-}

module Language.Marlowe.Runtime.Schema where

import Data.List.NonEmpty (NonEmpty (..))
import Hasql.DynamicSyntax.Ast
import Hasql.DynamicSyntax.Schema
import PostgresqlSyntax.Ast (Ident, JoinType (..), MathOp (..), SymbolicExprBinOp (..))

type BlockColumns =
  '[ '("id", SqlBytea, NotNull)
   , '("slotNo", SqlInt8, NotNull)
   , '("blockNo", SqlInt8, NotNull)
   , '("rollbackToBlock", SqlBytea, Null)
   , '("rollbackToSlot", SqlInt8, Null)
   ]

block :: Table BlockColumns
block = marloweTable "block"

type TxOutColumns =
  '[ '("txId", SqlBytea, NotNull)
   , '("txIx", SqlInt2, NotNull)
   , '("blockId", SqlBytea, NotNull)
   , '("address", SqlBytea, NotNull)
   , '("lovelace", SqlInt8, NotNull)
   ]

txOut :: Table TxOutColumns
txOut = marloweTable "txOut"

type TxOutAssetColumns =
  '[ '("txId", SqlBytea, NotNull)
   , '("txIx", SqlInt2, NotNull)
   , '("blockId", SqlBytea, NotNull)
   , '("policyId", SqlBytea, NotNull)
   , '("name", SqlBytea, NotNull)
   , '("quantity", SqlInt8, NotNull)
   ]

txOutAsset :: Table TxOutAssetColumns
txOutAsset = marloweTable "txOutAsset"

type ContractTxOutColumns =
  '[ '("txId", SqlBytea, NotNull)
   , '("txIx", SqlInt2, NotNull)
   , '("blockId", SqlBytea, NotNull)
   , '("payoutScriptHash", SqlBytea, NotNull)
   , '("contract", SqlBytea, NotNull)
   , '("state", SqlInt8, NotNull)
   , '("rolesCurrency", SqlBytea, NotNull)
   ]

contractTxOut :: Table ContractTxOutColumns
contractTxOut = marloweTable "contractTxOut"

type CreateTxOutColumns =
  '[ '("txId", SqlBytea, NotNull)
   , '("txIx", SqlInt2, NotNull)
   , '("blockId", SqlBytea, NotNull)
   , '("metadata", SqlBytea, Null)
   , '("slotNo", SqlInt8, NotNull)
   , '("blockNo", SqlInt8, NotNull)
   ]

createTxOut :: Table CreateTxOutColumns
createTxOut = marloweTable "createTxOut"

type PayoutTxOutColumns =
  '[ '("txId", SqlBytea, NotNull)
   , '("txIx", SqlInt2, NotNull)
   , '("blockId", SqlBytea, NotNull)
   , '("rolesCurrency", SqlBytea, NotNull)
   , '("role", SqlBytea, NotNull)
   ]

payoutTxOut :: Table PayoutTxOutColumns
payoutTxOut = marloweTable "payoutTxOut"

type ApplyTxColumns =
  '[ '("txId", SqlBytea, NotNull)
   , '("createTxId", SqlBytea, NotNull)
   , '("createTxIx", SqlInt2, NotNull)
   , '("blockId", SqlBytea, NotNull)
   , '("invalidBefore", SqlTimestamp, NotNull)
   , '("invalidHereafter", SqlTimestamp, NotNull)
   , '("metadata", SqlBytea, Null)
   , '("inputTxId", SqlBytea, NotNull)
   , '("inputTxIx", SqlInt2, NotNull)
   , '("inputs", SqlBytea, NotNull)
   , '("outputTxIx", SqlInt2, Null)
   , '("slotNo", SqlInt8, NotNull)
   , '("blockNo", SqlInt8, NotNull)
   ]

applyTx :: Table ApplyTxColumns
applyTx = marloweTable "applyTx"

type WithdrawalTxInColumns =
  '[ '("txId", SqlBytea, NotNull)
   , '("blockId", SqlBytea, NotNull)
   , '("payoutTxId", SqlBytea, NotNull)
   , '("payoutTxIx", SqlInt2, NotNull)
   , '("createTxId", SqlBytea, NotNull)
   , '("createTxIx", SqlInt2, NotNull)
   , '("slotNo", SqlInt8, NotNull)
   , '("blockNo", SqlInt8, NotNull)
   ]

withdrawalTxIn :: Table WithdrawalTxInColumns
withdrawalTxIn = marloweTable "withdrawalTxIn"

marloweTable :: (SingColumns cols) => Ident -> Table cols
marloweTable name = singTable name (Just "marlowe")

tempTable :: (SingColumns cols) => Ident -> Table cols
tempTable name = singTable name Nothing

-- * helpers

withCTEs :: Bool -> [CommonTableExpr] -> Maybe WithClause
withCTEs _ [] = Nothing
withCTEs recursive (x : xs) = Just $ WithClause recursive $ x :| xs

naturalJoin :: (IsTableRef a, IsTableRef b) => a -> b -> TableRef
naturalJoin = joinTable $ NaturalJoinMeth Nothing

innerJoinOn :: (IsAExpr expr, IsTableRef a, IsTableRef b) => expr -> a -> b -> TableRef
innerJoinOn expr = joinTable $ QualJoinMeth (Just InnerJoinType) $ OnJoinQual expr

leftJoinOn :: (IsAExpr expr, IsTableRef a, IsTableRef b) => expr -> a -> b -> TableRef
leftJoinOn expr = joinTable $ QualJoinMeth (Just $ LeftJoinType False) $ OnJoinQual expr

equals :: (IsAExpr a, IsAExpr b) => a -> b -> AExpr
equals l = SymbolicBinOpAExpr l (MathSymbolicExprBinOp EqualsMathOp)
