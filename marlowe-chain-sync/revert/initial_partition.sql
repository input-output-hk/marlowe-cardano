-- Revert chain:initial_partition from pg

BEGIN;

DROP TABLE chain.block_0;
DROP TABLE chain.block_1m;
DROP TABLE chain.block_2m;
DROP TABLE chain.block_3m;
DROP TABLE chain.block_4m;
DROP TABLE chain.block_5m;
DROP TABLE chain.block_6m;
DROP TABLE chain.block_7m;
DROP TABLE chain.block_8m;
DROP TABLE chain.block_9m;
DROP TABLE chain.block_10m;

DROP TABLE chain.tx_0;
DROP TABLE chain.tx_1m;
DROP TABLE chain.tx_2m;
DROP TABLE chain.tx_3m;
DROP TABLE chain.tx_4m;
DROP TABLE chain.tx_5m;
DROP TABLE chain.tx_6m;
DROP TABLE chain.tx_7m;
DROP TABLE chain.tx_8m;
DROP TABLE chain.tx_9m;
DROP TABLE chain.tx_10m;

DROP TABLE chain.txOut_0;
DROP TABLE chain.txOut_1m;
DROP TABLE chain.txOut_2m;
DROP TABLE chain.txOut_3m;
DROP TABLE chain.txOut_4m;
DROP TABLE chain.txOut_5m;
DROP TABLE chain.txOut_6m;
DROP TABLE chain.txOut_7m;
DROP TABLE chain.txOut_8m;
DROP TABLE chain.txOut_9m;
DROP TABLE chain.txOut_10m;

DROP TABLE chain.txIn_0;
DROP TABLE chain.txIn_1m;
DROP TABLE chain.txIn_2m;
DROP TABLE chain.txIn_3m;
DROP TABLE chain.txIn_4m;
DROP TABLE chain.txIn_5m;
DROP TABLE chain.txIn_6m;
DROP TABLE chain.txIn_7m;
DROP TABLE chain.txIn_8m;
DROP TABLE chain.txIn_9m;
DROP TABLE chain.txIn_10m;

COMMIT;
