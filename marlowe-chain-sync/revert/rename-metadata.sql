-- Revert chain:rename-metadata from pg

BEGIN;

ALTER TABLE chain.tx
RENAME metadata to metadataKey1564;

COMMIT;
