{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE QuasiQuotes #-}

module Language.Marlowe.Runtime.Sync.Database.PostgreSQL.GetNextHeaders
  where

import Data.Binary (get)
import Data.Binary.Get (runGet)
import Data.ByteString.Lazy (fromStrict)
import Data.Maybe (fromJust)
import qualified Data.Vector as V
import Hasql.TH (vectorStatement)
import qualified Hasql.Transaction as T
import Language.Marlowe.Runtime.ChainSync.Api
  ( Address(..)
  , BlockHeader(..)
  , BlockHeaderHash(..)
  , ChainPoint
  , Credential(..)
  , PolicyId(..)
  , ScriptHash(..)
  , TxId(..)
  , TxOutRef(..)
  , WithGenesis(..)
  , paymentCredential
  )
import Language.Marlowe.Runtime.Core.Api
  (ContractId(..), MarloweVersion(MarloweV1), SomeMarloweVersion(SomeMarloweVersion), emptyMarloweTransactionMetadata)
import Language.Marlowe.Runtime.Discovery.Api (ContractHeader(..))
import Language.Marlowe.Runtime.Sync.Database (Next(..))
import Language.Marlowe.Runtime.Sync.Database.PostgreSQL.GetNextSteps (Orientation(..), orient)
import Prelude hiding (init)

getNextHeaders :: ChainPoint -> T.Transaction (Next ContractHeader)
getNextHeaders point = do
  orient point >>= \case
    RolledBack toPoint -> pure $ Rollback toPoint
    AtTip -> pure Wait
    BeforeTip -> T.statement fromSlot $ decodeResult . V.toList <$>
      [vectorStatement|
        SELECT
          createTxOut.slotNo :: bigint,
          createTxOut.blockId :: bytea,
          createTxOut.blockNo :: bigint,
          createTxOut.txId :: bytea,
          createTxOut.txIx :: smallint,
          contractTxOut.rolesCurrency :: bytea,
          createTxOut.metadata :: bytea?,
          txOut.address :: bytea,
          contractTxOut.payoutScriptHash :: bytea
        FROM marlowe.createTxOut
        JOIN
          ( SELECT slotNo, blockId, blockNo
            FROM marlowe.createTxOut
            WHERE slotNo > $1 :: bigint
            ORDER BY slotNo
            LIMIT 1
          ) AS nextBlock USING (blockId)
        JOIN marlowe.contractTxOut USING (txId, txIx)
        JOIN marlowe.txOut USING (txId, txIx)
      |]
  where
    fromSlot = case point of
      Genesis -> -1
      At BlockHeader{..} -> fromIntegral slotNo

    decodeResult [] = Wait
    decodeResult (row : rows) =
      let
        blockHeader = decodeBlockHeader row
      in
        Next blockHeader $ decodeContractHeader blockHeader <$> row : rows

    decodeBlockHeader
      ( slot
      , hash
      , block
      , _
      , _
      , _
      , _
      , _
      , _
      ) = BlockHeader
        (fromIntegral slot)
        (BlockHeaderHash hash)
        (fromIntegral block)

    decodeContractHeader blockHeader
      ( _
      , _
      , _
      , txId
      , txIx
      , rolesCurrency
      , metadata
      , marloweScriptAddress
      , payoutScriptHash
      ) = ContractHeader
        { contractId = ContractId $ TxOutRef (TxId txId) (fromIntegral txIx)
        , rolesCurrency = PolicyId rolesCurrency
        , metadata = maybe emptyMarloweTransactionMetadata (runGet get . fromStrict) metadata
        , marloweScriptHash = fromJust do
            credential <- paymentCredential $ Address marloweScriptAddress
            case credential of
              ScriptCredential hash -> pure hash
              _ -> Nothing
        , marloweScriptAddress = Address marloweScriptAddress
        , payoutScriptHash = ScriptHash payoutScriptHash
        , marloweVersion = SomeMarloweVersion MarloweV1
        , blockHeader
        }
