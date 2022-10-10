-- Revert chain:split-address from pg

BEGIN;

DROP INDEX chain.txOut_addressPaymentCredential;
ALTER TABLE chain.txOut
  DROP COLUMN addressHeader,
  DROP COLUMN addressPaymentCredential,
  DROP COLUMN addressStakeAddressReference;

COMMIT;
