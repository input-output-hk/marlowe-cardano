-- Deploy chain:rename-metadata to pg
-- requires: tx

BEGIN;

ALTER TABLE chain.tx
RENAME metadataKey1564 to metadata;

COMMIT;
