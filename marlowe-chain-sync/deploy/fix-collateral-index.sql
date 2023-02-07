-- Deploy chain:fix-collateral-index to pg
-- requires: split-address

BEGIN;

-- Fix for `preview` testnet.
UPDATE chain.txOut
  SET txIx = 3
  WHERE (encode(txId, 'hex'), txIx) = ('5c4d3e1ad9921afb100040ed0fd275dcf7c2091d54b249a95ec00aa98bce9eff', 1);

-- Fix for `preview` testnet.
UPDATE chain.txOut
  SET txIx = 2
  WHERE (encode(txId, 'hex'), txIx) = ('a705221111f65ec604279e8c0d3b4b6d574139fae7db263f5a2afaac06d7d19c', 1);

COMMIT;
