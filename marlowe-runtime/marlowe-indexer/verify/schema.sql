-- Verify marlowe:schema on pg

BEGIN;

SELECT id, slotNo, blockNo, rollbackToBlock, rollbackToSlot
FROM marlowe.block
WHERE FALSE;

SELECT txId, txIx, address, lovelace
FROM marlowe.txOut
WHERE FALSE;

SELECT txId, txIx, policyId, name, quantity
FROM marlowe.txOutAsset
WHERE FALSE;

SELECT txId, txIx, contract, state, rolesCurrency
FROM marlowe.contractTxOut
WHERE FALSE;

SELECT txId, txIx, rolesCurrency, role
FROM marlowe.payoutTxOut
WHERE FALSE;

SELECT txId, txIx, blockId, payoutScriptHash, metadata
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

ROLLBACK;
