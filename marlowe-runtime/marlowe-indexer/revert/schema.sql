-- Revert marlowe:schema from pg

BEGIN;

DROP TABLE marlowe.withdrawalTxIn;
DROP TABLE marlowe.payoutTxOut;
DROP TABLE marlowe.applyTx;
DROP TABLE marlowe.createTxOut;
DROP TABLE marlowe.contractTxOut;
DROP TABLE marlowe.txOutAsset;
DROP TABLE marlowe.txOut;
DROP TABLE marlowe.block;
DROP SCHEMA marlowe;

COMMIT;
