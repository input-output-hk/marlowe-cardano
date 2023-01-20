-- Verify marlowe:schema on pg

BEGIN;

SELECT id, slotNo, blockNo, rollbackToBlock, rollbackToSlot
FROM marlowe.block
WHERE FALSE;

SELECT txId, txIx, blockId, address, lovelace
FROM marlowe.txOut
WHERE FALSE;

SELECT txId, txIx, blockId, policyId, name, quantity
FROM marlowe.txOutAsset
WHERE FALSE;

SELECT txId, txIx, blockId, payoutScriptHash, contract, state, rolesCurrency
FROM marlowe.contractTxOut
WHERE FALSE;

SELECT txId, txIx, blockId, rolesCurrency, role
FROM marlowe.payoutTxOut
WHERE FALSE;

SELECT txId, txIx, blockId, metadata
FROM marlowe.createTxOut
WHERE FALSE;

SELECT txId
     , createTxId
     , createTxIx
     , blockId
     , invalidBefore
     , invalidHereafter
     , metadata
     , inputTxId
     , inputTxIx
     , inputs
     , outputTxIx
FROM marlowe.applyTx
WHERE FALSE;

SELECT txId, blockId, payoutTxId, payoutTxIx
FROM marlowe.withdrawalTxIn
WHERE FALSE;

SELECT txId
     , inputTxId
     , inputTxIx
     , blockId
     , error
FROM marlowe.invalidApplyTx
WHERE FALSE;

ROLLBACK;
