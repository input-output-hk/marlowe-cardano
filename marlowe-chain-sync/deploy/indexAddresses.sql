-- Deploy chain:indexAddresses to pg
-- requires: split-address

BEGIN;

CREATE INDEX txOut_addressHeader_addressPaymentCredential ON chain.txOut USING BTREE (addressHeader, addressPaymentCredential);

COMMIT;
