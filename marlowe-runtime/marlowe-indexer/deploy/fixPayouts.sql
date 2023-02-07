-- Deploy marlowe:fixPayouts to pg
-- requires: schema

BEGIN;

UPDATE marlowe.payoutTxOut
SET
  rolesCurrency = role,
  role = rolesCurrency;

COMMIT;
