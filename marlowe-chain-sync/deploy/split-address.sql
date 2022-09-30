-- Deploy chain:split-address to pg
-- requires: md5address

BEGIN;

ALTER TABLE chain.txOut
  ADD COLUMN addressHeader bytea,
  ADD COLUMN addressPaymentCredential bytea,
  ADD COLUMN addressStakeAddressReference bytea;

UPDATE chain.txOut
  SET
    addressHeader = substring(address from 0 for 2),
    addressPaymentCredential =
      CASE
        WHEN length(address) IN (29, 57) THEN substring(address from 2 for 28)
        ELSE NULL
      END,
    addressStakeAddressReference =
      CASE
        WHEN length(address) = 57 THEN substring(address from 30)
        ELSE NULL
      END;

ALTER TABLE chain.txOut
  ALTER COLUMN addressHeader SET NOT NULL;

CREATE INDEX txOut_addressPaymentCredential ON chain.txOut USING BTREE (addressPaymentCredential);

COMMIT;
