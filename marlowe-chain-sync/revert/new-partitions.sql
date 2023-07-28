-- Revert chain:new-partitions from pg

BEGIN;

DROP TABLE chain.block_11m;
DROP TABLE chain.block_12m;
DROP TABLE chain.block_13m;
DROP TABLE chain.block_14m;
DROP TABLE chain.block_15m;
DROP TABLE chain.block_16m;
DROP TABLE chain.block_17m;
DROP TABLE chain.block_18m;
DROP TABLE chain.block_19m;
DROP TABLE chain.block_20m;

DROP TABLE chain.tx_11m;
DROP TABLE chain.tx_12m;
DROP TABLE chain.tx_13m;
DROP TABLE chain.tx_14m;
DROP TABLE chain.tx_15m;
DROP TABLE chain.tx_16m;
DROP TABLE chain.tx_17m;
DROP TABLE chain.tx_18m;
DROP TABLE chain.tx_19m;
DROP TABLE chain.tx_20m;

DROP TABLE chain.txOut_11m;
DROP TABLE chain.txOut_12m;
DROP TABLE chain.txOut_13m;
DROP TABLE chain.txOut_14m;
DROP TABLE chain.txOut_15m;
DROP TABLE chain.txOut_16m;
DROP TABLE chain.txOut_17m;
DROP TABLE chain.txOut_18m;
DROP TABLE chain.txOut_19m;
DROP TABLE chain.txOut_20m;

DROP TABLE chain.txIn_11m;
DROP TABLE chain.txIn_12m;
DROP TABLE chain.txIn_13m;
DROP TABLE chain.txIn_14m;
DROP TABLE chain.txIn_15m;
DROP TABLE chain.txIn_16m;
DROP TABLE chain.txIn_17m;
DROP TABLE chain.txIn_18m;
DROP TABLE chain.txIn_19m;
DROP TABLE chain.txIn_20m;

COMMIT;
