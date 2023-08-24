{-# LANGUAGE DataKinds #-}

module Language.Marlowe.Runtime.Schema where

import Hasql.DynamicSyntax.Ast
import Hasql.DynamicSyntax.Schema
import PostgresqlSyntax.Ast (Ident)

type BlockColumns =
  '[ 'Column "id" SqlBytea NotNull
   , 'Column "slotNo" SqlInt8 NotNull
   , 'Column "blockNo" SqlInt8 NotNull
   , 'Column "rollbackToBlock" SqlBytea Null
   , 'Column "rollbackToSlot" SqlInt8 Null
   ]

block :: Table BlockColumns
block = marloweTable "block"

type TxOutColumns =
  '[ 'Column "txId" SqlBytea NotNull
   , 'Column "txIx" SqlInt2 NotNull
   , 'Column "blockId" SqlBytea NotNull
   , 'Column "address" SqlBytea NotNull
   , 'Column "lovelace" SqlInt8 NotNull
   ]

txOut :: Table TxOutColumns
txOut = marloweTable "txOut"

type TxOutAssetColumns =
  '[ 'Column "txId" SqlBytea NotNull
   , 'Column "txIx" SqlInt2 NotNull
   , 'Column "blockId" SqlBytea NotNull
   , 'Column "policyId" SqlBytea NotNull
   , 'Column "name" SqlBytea NotNull
   , 'Column "quantity" SqlInt8 NotNull
   ]

txOutAsset :: Table TxOutAssetColumns
txOutAsset = marloweTable "txOutAsset"

type ContractTxOutAssetColumns =
  '[ 'Column "txId" SqlBytea NotNull
   , 'Column "txIx" SqlInt2 NotNull
   , 'Column "blockId" SqlBytea NotNull
   , 'Column "payoutScriptHash" SqlBytea NotNull
   , 'Column "contract" SqlBytea NotNull
   , 'Column "state" SqlInt8 NotNull
   , 'Column "rolesCurrency" SqlBytea NotNull
   ]

contractTxOutAsset :: Table ContractTxOutAssetColumns
contractTxOutAsset = marloweTable "contractTxOut"

type CreateTxOutAssetColumns =
  '[ 'Column "txId" SqlBytea NotNull
   , 'Column "txIx" SqlInt2 NotNull
   , 'Column "blockId" SqlBytea NotNull
   , 'Column "metadata" SqlBytea Null
   , 'Column "slotNo" SqlInt8 NotNull
   , 'Column "blockNo" SqlInt8 NotNull
   ]

createTxOutAsset :: Table CreateTxOutAssetColumns
createTxOutAsset = marloweTable "createTxOut"

type PayoutTxOutAssetColumns =
  '[ 'Column "txId" SqlBytea NotNull
   , 'Column "txIx" SqlInt2 NotNull
   , 'Column "blockId" SqlBytea NotNull
   , 'Column "rolesCurrency" SqlBytea NotNull
   , 'Column "role" SqlBytea NotNull
   ]

payoutTxOutAsset :: Table PayoutTxOutAssetColumns
payoutTxOutAsset = marloweTable "payoutTxOut"

type ApplyTxAssetColumns =
  '[ 'Column "txId" SqlBytea NotNull
   , 'Column "createTxId" SqlBytea NotNull
   , 'Column "createTxIx" SqlInt2 NotNull
   , 'Column "blockId" SqlBytea NotNull
   , 'Column "invalidBefore" SqlTimestamp NotNull
   , 'Column "invalidHereafter" SqlTimestamp NotNull
   , 'Column "metadata" SqlBytea Null
   , 'Column "inputTxId" SqlBytea NotNull
   , 'Column "inputTxIx" SqlInt2 NotNull
   , 'Column "inputs" SqlBytea NotNull
   , 'Column "outputTxIx" SqlInt2 Null
   , 'Column "slotNo" SqlInt8 NotNull
   , 'Column "blockNo" SqlInt8 NotNull
   ]

applyTxAsset :: Table ApplyTxAssetColumns
applyTxAsset = marloweTable "applyTx"

type WithdrawalTxInAssetColumns =
  '[ 'Column "txId" SqlBytea NotNull
   , 'Column "blockId" SqlBytea NotNull
   , 'Column "payoutTxId" SqlBytea NotNull
   , 'Column "payoutTxIx" SqlInt2 NotNull
   , 'Column "createTxId" SqlBytea NotNull
   , 'Column "createTxIx" SqlInt2 NotNull
   , 'Column "slotNo" SqlInt8 NotNull
   , 'Column "blockNo" SqlInt8 NotNull
   ]

withdrawalTxInAsset :: Table WithdrawalTxInAssetColumns
withdrawalTxInAsset = marloweTable "withdrawalTxIn"

marloweTable :: (SingColumns cols) => Ident -> Table cols
marloweTable name = Table name (Just "marlowe") singColumns

tempTable :: (SingColumns cols) => Ident -> Table cols
tempTable name = Table name Nothing singColumns
