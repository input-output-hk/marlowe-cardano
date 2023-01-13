{-# LANGUAGE GADTs #-}
{-# LANGUAGE QuasiQuotes #-}

module Language.Marlowe.Runtime.Indexer.Database.PostgreSQL.CommitBlocks
  where

import Data.Binary (put)
import Data.Binary.Put (runPut)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (toStrict)
import Data.Functor ((<&>))
import Data.Int (Int16, Int64)
import Data.List (unzip4, unzip5)
import qualified Data.Map as Map
import Data.Maybe (maybeToList)
import qualified Data.Set as Set
import Data.Time (LocalTime, utc, utcToLocalTime)
import Data.Vector (Vector)
import qualified Data.Vector as V
import Hasql.TH (resultlessStatement)
import qualified Hasql.Transaction as H
import Language.Marlowe.Core.V1.Semantics (MarloweData(..), MarloweParams(..))
import Language.Marlowe.Runtime.ChainSync.Api
import Language.Marlowe.Runtime.Core.Api (ContractId(..), MarloweVersion(..), TransactionScriptOutput(..))
import qualified Language.Marlowe.Runtime.Core.Api as Core
import Language.Marlowe.Runtime.History.Api (CreateStep(..), SomeCreateStep(..))
import Language.Marlowe.Runtime.Indexer.Types
import Plutus.V2.Ledger.Api (CurrencySymbol(..), fromBuiltin)

commitBlocks :: [MarloweBlock] -> H.Transaction ()
commitBlocks blocks = H.statement (prepareParams blocks)
  [resultlessStatement|
    WITH blockInputs (id, slotNo, blockNo) AS
      ( SELECT * FROM UNNEST ($1 :: bytea[], $2 :: bigint[], $3 :: bigint[])
      )
    , insertBlocks AS
      ( INSERT INTO marlowe.block (id, slotNo, blockNo)
        SELECT * FROM blockInputs
      )

    , txOutInputs (txId, txIx, address, lovelace) AS
      ( SELECT * FROM UNNEST ($4 :: bytea[], $5 :: smallint[], $6 :: bytea[], $7 :: bigint[])
      )
    , insertTxOuts AS
      ( INSERT INTO marlowe.txOut (txId, txIx, address, lovelace)
        SELECT * FROM txOutInputs
      )

    , txOutAssetInputs (txId, txIx, policyId, name, quantity) AS
      ( SELECT * FROM UNNEST ($8 :: bytea[], $9 :: smallint[], $10 :: bytea[],  $11 :: bytea[], $12 :: bigint[])
      )
    , insertTxOutAssets AS
      ( INSERT INTO marlowe.txOutAsset (txId, txIx, policyId, name, quantity)
        SELECT * FROM txOutAssetInputs
      )

    , contractTxOutInputs (txId, txIx, contract, state, rolesCurrency) AS
      ( SELECT * FROM UNNEST ($13 :: bytea[], $14 :: smallint[], $15 :: bytea[], $16 :: bytea[], $17 :: bytea[])
      )
    , insertContractTxOuts AS
      ( INSERT INTO marlowe.contractTxOut (txId, txIx, contract, state, rolesCurrency)
        SELECT * FROM contractTxOutInputs
      )

    , createTxOutInputs (txId, txIx, blockId, payoutScriptHash, metadata) AS
      ( SELECT * FROM UNNEST ($18 :: bytea[], $19 :: smallint[], $20 :: bytea[], $21 :: bytea[], $22 :: bytea?[])
      )
    , insertCreateTxOuts AS
      ( INSERT INTO marlowe.createTxOut (txId, txIx, blockId, payoutScriptHash, metadata)
        SELECT * FROM createTxOutInputs
      )

    , applyTxInputs (txId, createTxId, createTxIx, blockId, invalidBefore, invalidHereafter, metadata, inputTxId, inputTxIx, inputs, outputTxIx) AS
      ( SELECT * FROM UNNEST ($23 :: bytea[], $24 :: bytea[], $25 :: smallint[], $26 :: bytea[], $27 :: timestamp[], $28 :: timestamp[], $29 :: bytea?[], $30 :: bytea[], $31 :: smallint[], $32 :: bytea[], $33 :: smallint?[])
      )
    , insertApplyTxs AS
      ( INSERT INTO marlowe.applyTx (txId, createTxId, createTxIx, blockId, invalidBefore, invalidHereafter, metadata, inputTxId, inputTxIx, inputs, outputTxIx)
        SELECT * FROM applyTxInputs
      )

    , payoutTxOutInputs (txId, txIx, rolesCurrency, role) AS
      ( SELECT * FROM UNNEST ($34 :: bytea[], $35 :: smallint[], $36 :: bytea[], $37 :: bytea[])
      )
    , insertPayoutTxOuts AS
      ( INSERT INTO marlowe.payoutTxOut (txId, txIx, rolesCurrency, role)
        SELECT * FROM payoutTxOutInputs
      )

    , withdrawalTxInInputs (txId, blockId, payoutTxId, payoutTxIx) AS
      ( SELECT * FROM UNNEST ($38 :: bytea[], $39 :: bytea[], $40 :: bytea[], $41 :: smallint[])
      )
    INSERT INTO marlowe.withdrawalTxIn (txId, blockId, payoutTxId, payoutTxIx)
      SELECT * FROM withdrawalTxInInputs
  |]

type QueryParams =
  ( Vector ByteString -- block ID rows
  , Vector Int64 -- block slot no rows
  , Vector Int64 -- block block no rows

  , Vector ByteString -- txOut txId rows
  , Vector Int16 -- txOut txIx rows
  , Vector ByteString -- txOut address rows
  , Vector Int64 -- txOut lovelace rows

  , Vector ByteString -- txOutAsset txId rows
  , Vector Int16 -- txOutAsset txIx rows
  , Vector ByteString -- txOutAsset policyId rows
  , Vector ByteString -- txOutAsset name rows
  , Vector Int64 -- txOutAsset quantity rows

  , Vector ByteString -- contractTxOut txId rows
  , Vector Int16 -- contractTxOut txIx rows
  , Vector ByteString -- contractTxOut contract rows
  , Vector ByteString -- contractTxOut state rows
  , Vector ByteString -- contractTxOut rolesCurrency rows

  , Vector ByteString -- createTxOut txId rows
  , Vector Int16 -- createTxOut txIx rows
  , Vector ByteString -- createTxOut blockId rows
  , Vector ByteString -- createTxOut payoutScriptHash rows
  , Vector (Maybe ByteString) -- createTxOut metadata rows

  , Vector ByteString -- applyTx txId rows
  , Vector ByteString -- applyTx createTxId rows
  , Vector Int16 -- applyTx createTxIx rows
  , Vector ByteString -- applyTx blockId rows
  , Vector LocalTime -- applyTx invalidBefore rows
  , Vector LocalTime -- applyTx invalidHereafter rows
  , Vector (Maybe ByteString) -- applyTx metadata rows
  , Vector ByteString -- applyTx inputTxId rows
  , Vector Int16 -- applyTx inputTxIx rows
  , Vector ByteString -- applyTx inputs rows
  , Vector (Maybe Int16) -- applyTx outputTxIx rows

  , Vector ByteString -- payoutTxOut txId rows
  , Vector Int16 -- payoutTxOut txIx rows
  , Vector ByteString -- payoutTxOut rolesCurrency rows
  , Vector ByteString -- payoutTxOut role rows

  , Vector ByteString -- withdrawalTxIn txId rows
  , Vector ByteString -- withdrawalTxIn blockId rows
  , Vector ByteString -- withdrawalTxIn payoutTxId rows
  , Vector Int16 -- withdrawalTxIn payoutTxIx rows
  )

type BlockRow =
  ( ByteString -- ID
  , Int64 -- slot no
  , Int64 -- block no
  )

blockToRow :: BlockHeader -> BlockRow
blockToRow BlockHeader{..} =
  ( unBlockHeaderHash headerHash
  , fromIntegral slotNo
  , fromIntegral blockNo
  )

type TxOutRow =
  ( ByteString -- txId
  , Int16 -- txIx
  , ByteString -- address
  , Int64 -- lovelace
  )

type TxOutAssetRow =
  ( ByteString -- txId
  , Int16 -- txIx
  , ByteString -- policyId
  , ByteString -- name
  , Int64 -- quantity
  )

assetsToTxOutAssetRows
  :: ByteString
  -> Int16
  -> Assets
  -> [TxOutAssetRow]
assetsToTxOutAssetRows txId txIx Assets{..} =
  Map.toList (unTokens tokens) <&> \(assetId, quantity) ->
    ( txId
    , txIx
    , unPolicyId $ policyId assetId
    , unTokenName $ tokenName assetId
    , fromIntegral quantity
    )

type ContractTxOutRow =
  ( ByteString -- txId
  , Int16 -- txIx
  , ByteString -- contract
  , ByteString -- state
  , ByteString -- rolesCurrency
  )

transactionScriptOutputToRows
  :: TxOutRef
  -> Core.MarloweVersion v
  -> Core.TransactionScriptOutput v
  -> (ByteString, Int16, TxOutRow, ContractTxOutRow, [TxOutAssetRow])
transactionScriptOutputToRows TxOutRef{..} MarloweV1 TransactionScriptOutput{..} =
  ( txId'
  , txIx'
  , ( txId'
    , txIx'
    , unAddress address
    , fromIntegral $ ada assets
    )
  , ( txId'
    , txIx'
    , toStrict $ runPut $ put $ toDatum marloweContract
    , toStrict $ runPut $ put $ toDatum marloweState
    , fromBuiltin $ unCurrencySymbol $ rolesCurrency marloweParams
    )
  , assetsToTxOutAssetRows txId' txIx' assets
  )
  where
    MarloweData{..} = datum
    txId' = unTxId txId
    txIx' = fromIntegral txIx

type CreateTxOutRow =
  ( ByteString -- txId
  , Int16 -- txIx
  , ByteString -- blockId
  , ByteString -- payoutScriptHash
  , Maybe ByteString -- metadata
  )

createTxToTxOutRows :: BlockHeader -> MarloweCreateTransaction -> [(TxOutRow, ContractTxOutRow, CreateTxOutRow, [TxOutAssetRow])]
createTxToTxOutRows BlockHeader{..} MarloweCreateTransaction{..} =
  Map.toList newContracts <&> \(contractId, SomeCreateStep version CreateStep{..}) ->
    let
      (txId', txIx', txOutRow, contractTxOutRow, txOutAssetRows) =
        transactionScriptOutputToRows (unContractId contractId) version createOutput
    in
      ( txOutRow
      , contractTxOutRow
      , ( txId'
        , txIx'
        , unBlockHeaderHash headerHash
        , unScriptHash payoutValidatorHash
        , if Map.null (unTransactionMetadata metadata)
            then Nothing
            else Just $ toStrict $ runPut $ put metadata
        )
      , txOutAssetRows
      )

type ApplyTxRow =
  ( ByteString -- txId
  , ByteString -- createTxId
  , Int16 -- createTxIx
  , ByteString -- blockId
  , LocalTime -- invalidBefore
  , LocalTime -- invalidHereafter
  , Maybe ByteString -- metadata
  , ByteString -- inputTxId
  , Int16 -- inputTxIx
  , ByteString -- inputs
  , Maybe Int16 -- outputTxIx
  )

type PayoutTxOutRow =
  ( ByteString -- txId
  , Int16 -- txIx
  , ByteString -- rolesCurrency
  , ByteString -- role
  )

applyTxToRows
  :: BlockHeader
  -> MarloweApplyInputsTransaction
  -> ( ApplyTxRow
     , Maybe (TxOutRow, ContractTxOutRow, [TxOutAssetRow])
     , [(TxOutRow, PayoutTxOutRow, [TxOutAssetRow])]
     )
applyTxToRows BlockHeader{..} (MarloweApplyInputsTransaction MarloweV1 inputTxOutRef Core.Transaction{..}) =
  let
    txId' = unTxId transactionId
    (createTxId', createTxIx') = case unContractId contractId of
      TxOutRef{..} -> (unTxId txId, fromIntegral txIx)
    (inputTxId', inputTxIx') = case inputTxOutRef of
      TxOutRef{..} -> (unTxId txId, fromIntegral txIx)
    mOutputRows = Core.scriptOutput output <&> \scriptOutput ->
      transactionScriptOutputToRows (utxo scriptOutput) MarloweV1 scriptOutput
  in
    ( ( txId'
      , createTxId'
      , createTxIx'
      , unBlockHeaderHash headerHash
      , utcToLocalTime utc validityLowerBound
      , utcToLocalTime utc validityUpperBound
      , if Map.null (unTransactionMetadata metadata)
          then Nothing
          else Just $ toStrict $ runPut $ put metadata
      , inputTxId'
      , inputTxIx'
      , toStrict $ runPut $ put $ toDatum inputs
      , mOutputRows <&> \(_, txIx, _, _, _) -> txIx
      )
    , mOutputRows <&> \(_, _, txOutRow, contractTxOutRow, txOutAssetRows) ->
        (txOutRow, contractTxOutRow, txOutAssetRows)
    , Map.toList (Core.payouts output) <&> \(TxOutRef{..}, Core.Payout{..}) ->
        let
          txIx' = fromIntegral txIx
        in
          ( ( txId'
            , txIx'
            , unAddress address
            , fromIntegral $ ada assets
            )
          , ( txId'
            , txIx'
            , unTokenName $ tokenName datum
            , unPolicyId $ policyId datum
            )
          , assetsToTxOutAssetRows txId' txIx' assets
          )
    )

type WithdrawalTxInRow =
  ( ByteString -- txId
  , ByteString -- blockId
  , ByteString -- payoutTxId
  , Int16 -- payoutTxIx
  )

withdrawTxToWithdrawalTxInRows
  :: BlockHeader
  -> MarloweWithdrawTransaction
  -> [WithdrawalTxInRow]
withdrawTxToWithdrawalTxInRows BlockHeader{..} MarloweWithdrawTransaction{..} =
  Set.toList consumedPayouts <&> \TxOutRef{..} ->
    ( unTxId consumingTx
    , unBlockHeaderHash headerHash
    , unTxId txId
    , fromIntegral txIx
    )

prepareParams :: [MarloweBlock] -> QueryParams
prepareParams blocks =
  ( V.fromList blockIdRows
  , V.fromList blockSlotRows
  , V.fromList blockNoRows

  , V.fromList txOutTxIdRows
  , V.fromList txOutTxIxRows
  , V.fromList txOutAddressRows
  , V.fromList txOutLovelaceRows

  , V.fromList txOutAssetTxIdRows
  , V.fromList txOutAssetTxIxRows
  , V.fromList txOutAssetPolicyIdRows
  , V.fromList txOutAssetNameRows
  , V.fromList txOutAssetQuantityRows

  , V.fromList contractTxOutTxIdRows
  , V.fromList contractTxOutTxIxRows
  , V.fromList contractTxOutContractRows
  , V.fromList contractTxOutStateRows
  , V.fromList contractTxOutRolesCurrencyRows

  , V.fromList createTxOutTxIdRows
  , V.fromList createTxOutTxIxRows
  , V.fromList createTxOutBlockIdRows
  , V.fromList createTxOutPayoutScriptHashRows
  , V.fromList createTxOutMetadataRows

  , V.fromList applyTxTxIdRows
  , V.fromList applyTxCreateTxIdRows
  , V.fromList applyTxCreateTxIxRows
  , V.fromList applyTxBlockIdRows
  , V.fromList applyTxInvalidBeforeRows
  , V.fromList applyTxInvalidHereafterRows
  , V.fromList applyTxMetadataRows
  , V.fromList applyTxInputTxIdRows
  , V.fromList applyTxInputTxIxRows
  , V.fromList applyTxInputsRows
  , V.fromList applyTxOutputTxIxRows

  , V.fromList payoutTxOutTxIdRows
  , V.fromList payoutTxOutTxIxRows
  , V.fromList payoutTxOutRolesCurrencyRows
  , V.fromList payoutTxOutRoleRows

  , V.fromList withdrawalTxInTxIdRows
  , V.fromList withdrawalTxInBlockIdRows
  , V.fromList withdrawalTxInPayoutTxIdRows
  , V.fromList withdrawalTxInPayoutTxIxRows
  )
  where
    (blockIdRows, blockSlotRows, blockNoRows) = unzip3 $ blockToRow . blockHeader <$> blocks

    ( txOutRows
      , txOutAssetRows
      , contractTxOutRows
      , createTxOutRows
      , applyTxRows
      , payoutTxOutRows
      , withdrawalTxInRows
      ) = foldMap7 marloweBlockToTxRows blocks

    ( txOutTxIdRows
      , txOutTxIxRows
      , txOutAddressRows
      , txOutLovelaceRows
      ) = unzip4 txOutRows

    ( txOutAssetTxIdRows
      , txOutAssetTxIxRows
      , txOutAssetPolicyIdRows
      , txOutAssetNameRows
      , txOutAssetQuantityRows
      ) = unzip5 txOutAssetRows

    ( contractTxOutTxIdRows
      , contractTxOutTxIxRows
      , contractTxOutContractRows
      , contractTxOutStateRows
      , contractTxOutRolesCurrencyRows
      ) = unzip5 contractTxOutRows

    ( createTxOutTxIdRows
      , createTxOutTxIxRows
      , createTxOutBlockIdRows
      , createTxOutPayoutScriptHashRows
      , createTxOutMetadataRows
      ) = unzip5 createTxOutRows

    ( applyTxTxIdRows
      , applyTxCreateTxIdRows
      , applyTxCreateTxIxRows
      , applyTxBlockIdRows
      , applyTxInvalidBeforeRows
      , applyTxInvalidHereafterRows
      , applyTxMetadataRows
      , applyTxInputTxIdRows
      , applyTxInputTxIxRows
      , applyTxInputsRows
      , applyTxOutputTxIxRows
      ) = unzip11 applyTxRows

    ( payoutTxOutTxIdRows
      , payoutTxOutTxIxRows
      , payoutTxOutRolesCurrencyRows
      , payoutTxOutRoleRows
      ) = unzip4 payoutTxOutRows

    ( withdrawalTxInTxIdRows
      , withdrawalTxInBlockIdRows
      , withdrawalTxInPayoutTxIdRows
      , withdrawalTxInPayoutTxIxRows
      ) = unzip4 withdrawalTxInRows

marloweBlockToTxRows
  :: MarloweBlock
  -> ([TxOutRow], [TxOutAssetRow], [ContractTxOutRow], [CreateTxOutRow], [ApplyTxRow], [PayoutTxOutRow], [WithdrawalTxInRow])
marloweBlockToTxRows MarloweBlock{..} = flip foldMap7 transactions \case
  CreateTransaction tx ->
    let
      (txOutRows, contractTxOutRows, createTxOutRows, txOutAssetRows) = unzip4 $ createTxToTxOutRows blockHeader tx
    in
      (txOutRows, concat txOutAssetRows, contractTxOutRows, createTxOutRows, [], [], [])
  ApplyInputsTransaction tx ->
    let
      (applyTxRow, mOutRows, payoutRows) = applyTxToRows blockHeader tx
      (txOutRows, contractTxOutRows, txOutAssetRows) = unzip3 $ maybeToList mOutRows
      (txOutRows', payoutTxOutRows, txOutAssetRows') = unzip3 payoutRows
    in
      ( txOutRows <> txOutRows'
      , concat (txOutAssetRows <> txOutAssetRows')
      , contractTxOutRows
      , []
      , [applyTxRow]
      , payoutTxOutRows
      , []
      )
  WithdrawTransaction tx -> ([], [], [], [] , [], [], withdrawTxToWithdrawalTxInRows blockHeader tx)
  _ -> ([], [], [], [] , [], [], [])


foldMap7
  :: (Monoid a, Monoid b, Monoid c, Monoid d, Monoid e, Monoid f, Monoid g, Foldable t)
  => (x -> (a, b, c, d, e, f, g))
  -> t x
  -> (a, b, c, d, e, f, g)
foldMap7 k = foldr
  ( \x (a', b', c', d', e', f', g') ->
    let
      (a, b, c, d, e, f, g) = k x
    in
      ( a <> a'
      , b <> b'
      , c <> c'
      , d <> d'
      , e <> e'
      , f <> f'
      , g <> g'
      )
  )
  (mempty, mempty, mempty, mempty, mempty, mempty, mempty)

unzip11 :: [(a, b, c, d, e, f, g, h, i, j, k)] -> ([a], [b], [c], [d], [e], [f], [g], [h], [i], [j], [k])
unzip11 = foldr
  (\(a, b, c, d, e, f, g, h, i, j, k) (as, bs, cs, ds, es, fs, gs, hs, is, js, ks) ->
    ( a : as
    , b : bs
    , c : cs
    , d : ds
    , e : es
    , f : fs
    , g : gs
    , h : hs
    , i : is
    , j : js
    , k : ks
    )
  )
  ([], [], [], [], [], [], [], [], [], [], [])
