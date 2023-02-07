-- Revert marlowe:withdrawalCreateNotNull from pg

BEGIN;

ALTER TABLE marlowe.withdrawalTxIn
  ALTER COLUMN createTxId DROP NOT NULL,
  ALTER COLUMN createTxIx DROP NOT NULL;

COMMIT;
