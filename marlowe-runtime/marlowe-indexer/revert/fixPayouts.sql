-- Revert marlowe:fixPayouts from pg

BEGIN;

UPDATE marlowe.payoutTxOut
SET
  rolesCurrency = role,
  role = rolesCurrency;

COMMIT;
