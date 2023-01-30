{-# LANGUAGE QuasiQuotes #-}

module Language.Marlowe.Runtime.Sync.Database.PostgreSQL.GetTip
  where

import Hasql.TH (maybeStatement)
import qualified Hasql.Transaction as T
import Language.Marlowe.Runtime.ChainSync.Api (BlockHeader(..), BlockHeaderHash(..), ChainPoint, WithGenesis(..))

getTip :: T.Transaction ChainPoint
getTip = T.statement () $ decodePoint <$>
  [maybeStatement|
    SELECT
      block.slotNo :: bigint,
      block.id :: bytea,
      block.blockNo :: bigint
    FROM marlowe.block
    ORDER BY block.slotNo DESC
    LIMIT 1
  |]
  where
    decodePoint = \case
      Nothing -> Genesis
      Just (slot, hash, block) -> At $ BlockHeader
        (fromIntegral slot)
        (BlockHeaderHash hash)
        (fromIntegral block)
