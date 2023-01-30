{-# LANGUAGE QuasiQuotes #-}

module Language.Marlowe.Runtime.Sync.Database.PostgreSQL.GetIntersection
  where

import Data.ByteString (ByteString)
import Data.Int (Int64)
import Data.Maybe (listToMaybe)
import qualified Data.Vector as V
import Hasql.TH (vectorStatement)
import qualified Hasql.Transaction as T
import Language.Marlowe.Runtime.ChainSync.Api (BlockHeader(..), BlockHeaderHash(..))

getIntersection :: [BlockHeader] -> T.Transaction (Maybe BlockHeader)
getIntersection [] = pure Nothing
getIntersection (b : bs) = do
  serverBlocks <- fmap decodeBlock . V.toList <$> T.statement (fromIntegral $ slotNo b)
    [vectorStatement|
      SELECT
        block.slotNo :: bigint,
        block.id :: bytea,
        block.blockNo :: bigint
      FROM marlowe.block
      WHERE slotNo >= $1 :: bigint
      ORDER BY slotNo
    |]
  pure
    $ fmap fst
    $ listToMaybe
    $ reverse
    $ takeWhile (uncurry (==))
    $ zip (b : bs)
    $ dropWhile (/= b) serverBlocks

decodeBlock :: (Int64, ByteString, Int64) -> BlockHeader
decodeBlock (slot, hash, block) = BlockHeader
  (fromIntegral slot)
  (BlockHeaderHash hash)
  (fromIntegral block)
