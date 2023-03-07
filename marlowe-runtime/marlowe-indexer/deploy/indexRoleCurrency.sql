-- Deploy marlowe:indexRoleCurrency to pg
-- requires: schema

BEGIN;

CREATE INDEX contractTxOut_rolesCurrency ON marlowe.contractTxOut USING BTREE (rolesCurrency);
CREATE INDEX payoutTxOut_rolesCurrency ON marlowe.payoutTxOut USING BTREE (rolesCurrency);
CREATE INDEX payoutTxOut_rolesCurrency_role ON marlowe.payoutTxOut USING BTREE (rolesCurrency, role);

COMMIT;
