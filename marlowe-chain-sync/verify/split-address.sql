-- Verify chain:split-address on pg

BEGIN;

CREATE OR REPLACE FUNCTION verify() RETURNS void AS $$
BEGIN
    PERFORM *
    FROM chain.txOut
    WHERE addressPaymentCredential IS NOT NULL AND
      (  addressStakeAddressReference IS NULL AND address != addressHeader || addressPaymentCredential
      OR addressStakeAddressReference IS NOT NULL AND address != addressHeader || addressPaymentCredential || addressStakeAddressReference
      );
    IF FOUND THEN
      RAISE EXCEPTION 'found bad rows';
    END IF;
END;
$$ Language plpgsql;

ROLLBACK;
