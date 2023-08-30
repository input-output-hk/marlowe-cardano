{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes #-}

module Language.Marlowe.Runtime.Indexer.Party (
  ContractTxOutParty (..),
  commitParties,
  indexParties,
) where

import Colog (WithLog, logError, logInfo)
import Colog.Message (Message)
import Control.Exception (Exception (..))
import Control.Monad (join)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Maybe (MaybeT (..))
import Data.Binary (Binary (..))
import Data.Binary.Get (runGet)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.Foldable (Foldable (..))
import Data.Int (Int16, Int64)
import Data.Maybe (fromJust)
import Data.Semigroup (Max (..))
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Vector as V
import Hasql.Connection (Connection)
import qualified Hasql.Session as Session
import Hasql.TH (resultlessStatement, singletonStatement, vectorStatement)
import qualified Hasql.Transaction as T
import qualified Hasql.Transaction.Sessions as T
import Language.Marlowe.Core.V1.Plate (Extract (..))
import Language.Marlowe.Core.V1.Semantics.Types (Contract, Party (..))
import Language.Marlowe.Core.V1.Semantics.Types.Address (serialiseAddress)
import Language.Marlowe.Runtime.ChainSync.Api (SlotNo (..), TxId (..), TxOutRef (..), fromDatum)
import Language.Marlowe.Runtime.Core.Api (ContractId (..))
import qualified Plutus.V2.Ledger.Api as PV2
import UnliftIO (throwIO)

indexParties :: (MonadIO m, WithLog env Message m) => Connection -> m ()
indexParties connection = do
  logInfo "Indexing contract parties"
  either logQueryError pure =<< liftIO (Session.run (mainLoop Nothing) connection)
  logInfo "Contract parties indexed"

logQueryError :: (WithLog env Message m, MonadIO m) => Session.QueryError -> m ()
logQueryError e = do
  logError $ "Failed to index parties: " <> T.pack (displayException e)
  throwIO e

mainLoop :: Maybe SlotNo -> Session.Session ()
mainLoop fromSlot = join $ T.transaction T.Serializable T.Write do
  contractTxOuts <- loadContracts fromSlot
  case foldMap (Just . toEntries) contractTxOuts of
    Nothing -> pure $ pure ()
    Just (Max nextSlot, entries) -> do
      commitParties entries
      pure $ mainLoop $ Just nextSlot

loadContracts :: Maybe SlotNo -> T.Transaction [ContractTxOut]
loadContracts fromSlot =
  fold <$> runMaybeT do
    fromSlot' <- maybe (MaybeT $ T.statement () firstSlotStatement) (pure . fromIntegral) fromSlot
    lift $ V.toList . fmap decodeContractTxOut <$> T.statement fromSlot' statement
  where
    firstSlotStatement =
      [singletonStatement|
        WITH maxIndexedBlock (slotNo) AS
          ( SELECT COALESCE(MAX(block.slotNo), -1)
            FROM marlowe.block
            NATURAL JOIN marlowe.contractTxOut
            NATURAL JOIN marlowe.contractTxOutPartyAddress
            NATURAL JOIN marlowe.contractTxOutPartyRole
          )
        SELECT MIN(block.slotNo) :: bigint?
        FROM marlowe.block, maxIndexedBlock
        WHERE block.slotNo > maxIndexedBlock.slotNo
      |]
    statement =
      [vectorStatement|
        WITH blocks (blockId, slotNo) AS
          ( SELECT id, slotNo
            FROM marlowe.block
            WHERE slotNo > $1 :: bigint
            ORDER BY slotNo
            LIMIT (1024 * 16)
          )
        SELECT
          contractTxOut.txId :: bytea,
          contractTxOut.txIx :: smallint,
          COALESCE(createTxOut.txId, applyTx.createTxId) :: bytea,
          COALESCE(createTxOut.txIx, applyTx.createTxIx) :: smallint,
          contractTxOut.contract :: bytea,
          blocks.slotNo :: bigint,
          contractTxOut.rolesCurrency :: bytea
        FROM marlowe.contractTxOut
        NATURAL JOIN blocks
        NATURAL LEFT JOIN marlowe.createTxOut
        LEFT JOIN marlowe.applyTx
          ON applyTx.txId = contractTxOut.txId
        LEFT JOIN marlowe.contractTxOutPartyAddress
          ON contractTxOutPartyAddress.txId = contractTxOut.txId
          AND contractTxOutPartyAddress.txIx = contractTxOut.txIx
        LEFT JOIN marlowe.contractTxOutPartyRole
          ON contractTxOutPartyRole.txId = contractTxOut.txId
          AND contractTxOutPartyRole.txIx = contractTxOut.txIx
        WHERE contractTxOutPartyAddress.txId IS NULL
          AND contractTxOutPartyRole.txId IS NULL
      |]

decodeContractTxOut :: (ByteString, Int16, ByteString, Int16, ByteString, Int64, ByteString) -> ContractTxOut
decodeContractTxOut (txId, txIx, createTxId, createTxIx, contract, slotNo, rolesCurrency) =
  ContractTxOut
    { contractOut = TxOutRef (TxId txId) (fromIntegral txIx)
    , contractId = ContractId $ TxOutRef (TxId createTxId) (fromIntegral createTxIx)
    , contract = fromJust $ fromDatum $ runGet get $ LBS.fromStrict contract
    , slotNo = fromIntegral slotNo
    , rolesCurrency
    }

toEntries :: ContractTxOut -> (Max SlotNo, [ContractTxOutParty])
toEntries ContractTxOut{..} =
  ( Max slotNo
  , [ContractTxOutParty{..} | party <- Set.toList $ extractAll contract]
  )

commitParties :: [ContractTxOutParty] -> T.Transaction ()
commitParties parties =
  T.statement params statement
  where
    statement =
      [resultlessStatement|
        WITH addressParties (address, txId, txIx, createTxId, createTxIx) AS
          ( SELECT * FROM UNNEST
              ( $1 :: bytea[]
              , $2 :: bytea[]
              , $3 :: smallint[]
              , $4 :: bytea[]
              , $5 :: smallint[]
              )
          )
        , roleParties (rolesCurrency, role, txId, txIx, createTxId, createTxIx) AS
          ( SELECT * FROM UNNEST
              ( $6 :: bytea[]
              , $7 :: bytea[]
              , $8 :: bytea[]
              , $9 :: smallint[]
              , $10 :: bytea[]
              , $11 :: smallint[]
              )
          )
        , addressInserts AS
          ( INSERT INTO marlowe.contractTxOutPartyAddress (address, txId, txIx, createTxId, createTxIx)
            SELECT * from addressParties
          )
        INSERT INTO marlowe.contractTxOutPartyRole (rolesCurrency, role, txId, txIx, createTxId, createTxIx)
        SELECT * from roleParties
      |]
    partyVector = V.fromList parties
    params =
      ( addresses
      , addressTxIds
      , addressTxIxs
      , addressCreateTxIds
      , addressCreateTxIxs
      , roleCurrencies
      , roles
      , roleTxIds
      , roleTxIxs
      , roleCreateTxIds
      , roleCreateTxIxs
      )
    ( addresses
      , addressTxIds
      , addressTxIxs
      , addressCreateTxIds
      , addressCreateTxIxs
      ) = V.unzip5 $ V.mapMaybe encodeAddress partyVector
    ( roleCurrencies
      , roles
      , roleTxIds
      , roleTxIxs
      , roleCreateTxIds
      , roleCreateTxIxs
      ) = V.unzip6 $ V.mapMaybe encodeRole partyVector

encodeAddress :: ContractTxOutParty -> Maybe (ByteString, ByteString, Int16, ByteString, Int16)
encodeAddress ContractTxOutParty{..} = case party of
  Address network address ->
    Just
      ( serialiseAddress network address
      , unTxId $ txId contractOut
      , fromIntegral $ txIx contractOut
      , unTxId $ txId $ unContractId contractId
      , fromIntegral $ txIx $ unContractId contractId
      )
  _ -> Nothing

encodeRole :: ContractTxOutParty -> Maybe (ByteString, ByteString, ByteString, Int16, ByteString, Int16)
encodeRole ContractTxOutParty{..} = case party of
  Role (PV2.TokenName role) ->
    Just
      ( rolesCurrency
      , PV2.fromBuiltin role
      , unTxId $ txId contractOut
      , fromIntegral $ txIx contractOut
      , unTxId $ txId $ unContractId contractId
      , fromIntegral $ txIx $ unContractId contractId
      )
  _ -> Nothing

data ContractTxOut = ContractTxOut
  { contractOut :: TxOutRef
  , contractId :: ContractId
  , contract :: Contract
  , rolesCurrency :: ByteString
  , slotNo :: SlotNo
  }

data ContractTxOutParty = ContractTxOutParty
  { contractOut :: TxOutRef
  , contractId :: ContractId
  , rolesCurrency :: ByteString
  , party :: Party
  }
