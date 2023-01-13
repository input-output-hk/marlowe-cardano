{-# LANGUAGE QuasiQuotes #-}

module Language.Marlowe.Runtime.Indexer.Database.PostgreSQL.GetIntersectionPoints
  where

import Data.ByteString (ByteString)
import Data.Int (Int64)
import qualified Data.Vector as V
import Hasql.TH (singletonStatement, vectorStatement)
import qualified Hasql.Transaction as H
import Language.Marlowe.Runtime.ChainSync.Api

getIntersectionPoints :: Int -> H.Transaction [BlockHeader]
getIntersectionPoints securityParameter = do
  mTipBlockNo <- H.statement () [singletonStatement|
    SELECT MAX(blockNo) :: bigint? from marlowe.block
  |]
  case mTipBlockNo of
    Nothing -> pure []
    Just tipBlockNo -> fmap decodeResultRow . V.toList <$> H.statement (tipBlockNo - fromIntegral securityParameter) [vectorStatement|
      SELECT id :: bytea, slotNo :: bigint, blockNo :: bigint
        FROM marlowe.block
       WHERE blockNo >= $1 :: bigint
       ORDER BY slotNo DESC
    |]

decodeResultRow :: (ByteString, Int64, Int64) -> BlockHeader
decodeResultRow (hash, slotNo, blockNo) =
  BlockHeader (fromIntegral slotNo) (BlockHeaderHash hash) (fromIntegral blockNo)
