-- Revert chain:indexAddresses from pg

BEGIN;

DROP INDEX txOut_addressHeader_addressPaymentCredential;

COMMIT;
