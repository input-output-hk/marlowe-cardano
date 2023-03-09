{-# LANGUAGE QuasiQuotes #-}

module Language.Marlowe.Runtime.Sync.Database.PostgreSQL.GetCreateStep
  where

import Control.Foldl (Fold(Fold))
import Data.Binary (get)
import Data.Binary.Get (runGet)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (fromStrict)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Hasql.TH (foldStatement)
import qualified Hasql.Transaction as T
import qualified Language.Marlowe.Core.V1.Semantics as V1
import Language.Marlowe.Runtime.ChainSync.Api
  ( Address(..)
  , AssetId(..)
  , Assets(..)
  , BlockHeader(..)
  , BlockHeaderHash(..)
  , PolicyId(..)
  , ScriptHash(..)
  , TokenName(..)
  , Tokens(..)
  , TxId(..)
  , TxOutRef(..)
  , fromDatum
  )
import Language.Marlowe.Runtime.Core.Api
  (ContractId(..), MarloweVersion(..), TransactionScriptOutput(..), emptyMarloweTransactionMetadata)
import Language.Marlowe.Runtime.History.Api (CreateStep(..), SomeCreateStep(..))
import qualified Plutus.V2.Ledger.Api as PV2
import Prelude hiding (init)

getCreateStep :: ContractId -> T.Transaction (Maybe (BlockHeader, SomeCreateStep))
getCreateStep (ContractId TxOutRef{..}) = T.statement params $
  [foldStatement|
    WITH contractId (txId, txIx) AS
      ( SELECT $1 :: bytea, $2 :: smallint
      )
    SELECT
      createTxOut.slotNo :: bigint,
      createTxOut.blockId :: bytea,
      createTxOut.blockNo :: bigint,
      txOut.address :: bytea,
      txOut.lovelace :: bigint,
      txOutAsset.policyId :: bytea?,
      txOutAsset.name :: bytea?,
      txOutAsset.quantity :: bigint?,
      contractTxOut.payoutScriptHash :: bytea,
      contractTxOut.contract :: bytea,
      contractTxOut.state :: bytea,
      contractTxOut.rolesCurrency :: bytea,
      createTxOut.metadata :: bytea?
    FROM marlowe.createTxOut
    JOIN marlowe.contractTxOut USING (txId, txIx)
    JOIN marlowe.txOut USING (txId, txIx)
    LEFT JOIN marlowe.txOutAsset USING (txId, txIx)
    JOIN contractId USING (txId, txIx)
  |] foldResults
  where
    params = (unTxId txId, fromIntegral txIx)
    foldResults = Fold (fmap Just . foldRow) Nothing id
    foldRow Nothing row = foldRowAssets row <$> extractBlockAndStep row
    foldRow (Just (block, step)) row = (block, foldRowAssets row step)

    extractBlockAndStep
      ( slot
      , headerHash
      , block
      , address
      , lovelace
      , mPolicyId
      , mName
      , mQuantity
      , payoutScriptHash
      , contract
      , state
      , rolesCurrency :: ByteString
      , metadata
      ) =
        ( BlockHeader
            (fromIntegral slot)
            (BlockHeaderHash headerHash)
            (fromIntegral block)
        , SomeCreateStep MarloweV1 CreateStep
            { createOutput = TransactionScriptOutput
                { address = Address address
                , assets = Assets
                    { ada = fromIntegral lovelace
                    , tokens = Tokens case (mPolicyId, mName, mQuantity) of
                        (Just policyId, Just name, Just quantity) -> Map.singleton
                          (AssetId (PolicyId policyId) (TokenName name))
                          (fromIntegral quantity)
                        _ -> mempty
                    }
                , utxo = TxOutRef{..}
                , datum = V1.MarloweData
                    { marloweParams = V1.MarloweParams $ PV2.CurrencySymbol $ PV2.toBuiltin rolesCurrency
                    , marloweState = fromJust $ fromDatum $ runGet get $ fromStrict state
                    , marloweContract = fromJust $ fromDatum $ runGet get $ fromStrict contract
                    }
                }
            , metadata = maybe emptyMarloweTransactionMetadata (runGet get . fromStrict) metadata
            , payoutValidatorHash = ScriptHash payoutScriptHash
            }
        )

    foldRowAssets
      ( _
      , _
      , _
      , _
      , _
      , Just policyId
      , Just name
      , Just quantity
      , _
      , _
      , _
      , _
      , _
      )
      (SomeCreateStep version createStep) = SomeCreateStep version createStep
        { createOutput = let output = createOutput createStep in output
            { assets = let oldAssets = assets output in oldAssets
                { tokens = Tokens let Tokens oldTokens = tokens oldAssets in Map.insert
                    (AssetId (PolicyId policyId) (TokenName name))
                    (fromIntegral quantity)
                    oldTokens
                }
            }
        }
    foldRowAssets _ step = step
