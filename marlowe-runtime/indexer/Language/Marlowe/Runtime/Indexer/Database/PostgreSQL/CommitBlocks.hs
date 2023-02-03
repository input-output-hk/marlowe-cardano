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
import Data.List (unzip4, unzip5, unzip6, unzip7)
import qualified Data.Map as Map
import Data.Maybe (maybeToList)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
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

    , txOutInputs (txId, txIx, blockId, address, lovelace) AS
      ( SELECT * FROM UNNEST ($4 :: bytea[], $5 :: smallint[], $6 :: bytea[], $7 :: bytea[], $8 :: bigint[])
      )
    , insertTxOuts AS
      ( INSERT INTO marlowe.txOut (txId, txIx, blockId, address, lovelace)
        SELECT * FROM txOutInputs
      )

    , txOutAssetInputs (txId, txIx, blockId, policyId, name, quantity) AS
      ( SELECT * FROM UNNEST ($9 :: bytea[], $10 :: smallint[], $11 :: bytea[], $12 :: bytea[],  $13 :: bytea[], $14 :: bigint[])
      )
    , insertTxOutAssets AS
      ( INSERT INTO marlowe.txOutAsset (txId, txIx, blockId, policyId, name, quantity)
        SELECT * FROM txOutAssetInputs
      )

    , contractTxOutInputs (txId, txIx, blockId, payoutScriptHash, contract, state, rolesCurrency) AS
      ( SELECT * FROM UNNEST ($15 :: bytea[], $16 :: smallint[], $17 :: bytea[], $18 :: bytea[], $19 :: bytea[], $20 :: bytea[], $21 :: bytea[])
      )
    , insertContractTxOuts AS
      ( INSERT INTO marlowe.contractTxOut (txId, txIx, blockId, payoutScriptHash, contract, state, rolesCurrency)
        SELECT * FROM contractTxOutInputs
      )

    , createTxOutInputs (txId, txIx, slotNo, blockId, blockNo, metadata) AS
      ( SELECT * FROM UNNEST ($22 :: bytea[], $23 :: smallint[], $24 :: bigint[], $25 :: bytea[], $26 :: bigint[], $27 :: bytea?[])
      )
    , insertCreateTxOuts AS
      ( INSERT INTO marlowe.createTxOut (txId, txIx, slotNo, blockId, blockNo, metadata)
        SELECT * FROM createTxOutInputs
      )

    , applyTxInputs (txId, createTxId, createTxIx, slotNo, blockId, blockNo, invalidBefore, invalidHereafter, metadata, inputTxId, inputTxIx, inputs, outputTxIx) AS
      ( SELECT * FROM UNNEST ($28 :: bytea[], $29 :: bytea[], $30 :: smallint[], $31 :: bigint[], $32 :: bytea[], $33 :: bigint[], $34 :: timestamp[], $35 :: timestamp[], $36 :: bytea?[], $37 :: bytea[], $38 :: smallint[], $39 :: bytea[], $40 :: smallint?[])
      )
    , insertApplyTxs AS
      ( INSERT INTO marlowe.applyTx (txId, createTxId, createTxIx, slotNo, blockId, blockNo, invalidBefore, invalidHereafter, metadata, inputTxId, inputTxIx, inputs, outputTxIx)
        SELECT * FROM applyTxInputs
      )

    , payoutTxOutInputs (txId, txIx, blockId, rolesCurrency, role) AS
      ( SELECT * FROM UNNEST ($41 :: bytea[], $42 :: smallint[], $43 :: bytea[], $44 :: bytea[], $45 :: bytea[])
      )
    , insertPayoutTxOuts AS
      ( INSERT INTO marlowe.payoutTxOut (txId, txIx, blockId, rolesCurrency, role)
        SELECT * FROM payoutTxOutInputs
      )

    , withdrawalTxInInputs (txId, slotNo, blockId, blockNo, payoutTxId, payoutTxIx) AS
      ( SELECT * FROM UNNEST ($46 :: bytea[], $47 :: bigint[], $48 :: bytea[], $49 :: bigint[], $50 :: bytea[], $51 :: smallint[])
      )
    , insertWithdrawalTxIns AS
      ( INSERT INTO marlowe.withdrawalTxIn (txId, slotNo, blockId, blockNo, payoutTxId, payoutTxIx)
        SELECT * FROM withdrawalTxInInputs
      )
    , invalidApplyTxInputs (txId, inputTxId, inputTxIx, blockId, error) AS
      ( SELECT * FROM UNNEST ($52 :: bytea[], $53 :: bytea[], $54 :: smallint[], $55 :: bytea[], $56 :: text[])
      )
    INSERT INTO marlowe.invalidApplyTx (txId, inputTxId, inputTxIx, blockId, error)
    SELECT * FROM invalidApplyTxInputs
  |]

type QueryParams =
  ( Vector ByteString -- block ID rows
  , Vector Int64 -- block slot no rows
  , Vector Int64 -- block block no rows

  , Vector ByteString -- txOut txId rows
  , Vector Int16 -- txOut txIx rows
  , Vector ByteString -- txOut blockId rows
  , Vector ByteString -- txOut address rows
  , Vector Int64 -- txOut lovelace rows

  , Vector ByteString -- txOutAsset txId rows
  , Vector Int16 -- txOutAsset txIx rows
  , Vector ByteString -- txOutAsset blockId rows
  , Vector ByteString -- txOutAsset policyId rows
  , Vector ByteString -- txOutAsset name rows
  , Vector Int64 -- txOutAsset quantity rows

  , Vector ByteString -- contractTxOut txId rows
  , Vector Int16 -- contractTxOut txIx rows
  , Vector ByteString -- contractTxOut blockId rows
  , Vector ByteString -- createTxOut payoutScriptHash rows
  , Vector ByteString -- contractTxOut contract rows
  , Vector ByteString -- contractTxOut state rows
  , Vector ByteString -- contractTxOut rolesCurrency rows

  , Vector ByteString -- createTxOut txId rows
  , Vector Int16 -- createTxOut txIx rows
  , Vector Int64 -- createTxOut slotNo rows
  , Vector ByteString -- createTxOut blockId rows
  , Vector Int64 -- createTxOut blockNo rows
  , Vector (Maybe ByteString) -- createTxOut metadata rows

  , Vector ByteString -- applyTx txId rows
  , Vector ByteString -- applyTx createTxId rows
  , Vector Int16 -- applyTx createTxIx rows
  , Vector Int64 -- applyTx slotNo rows
  , Vector ByteString -- applyTx blockId rows
  , Vector Int64 -- applyTx blockNo rows
  , Vector LocalTime -- applyTx invalidBefore rows
  , Vector LocalTime -- applyTx invalidHereafter rows
  , Vector (Maybe ByteString) -- applyTx metadata rows
  , Vector ByteString -- applyTx inputTxId rows
  , Vector Int16 -- applyTx inputTxIx rows
  , Vector ByteString -- applyTx inputs rows
  , Vector (Maybe Int16) -- applyTx outputTxIx rows

  , Vector ByteString -- payoutTxOut txId rows
  , Vector Int16 -- payoutTxOut txIx rows
  , Vector ByteString -- payoutTxOut blockId rows
  , Vector ByteString -- payoutTxOut rolesCurrency rows
  , Vector ByteString -- payoutTxOut role rows

  , Vector ByteString -- withdrawalTxIn txId rows
  , Vector Int64 -- withdrawalTxIn slotNo rows
  , Vector ByteString -- withdrawalTxIn blockId rows
  , Vector Int64 -- withdrawalTxIn blockNo rows
  , Vector ByteString -- withdrawalTxIn payoutTxId rows
  , Vector Int16 -- withdrawalTxIn payoutTxIx rows

  , Vector ByteString -- invalidApplyTx txId rows
  , Vector ByteString -- invalidApplyTx inputTxId rows
  , Vector Int16 -- invalidApplyTx inputTxIx rows
  , Vector ByteString -- invalidApplyTx blockId rows
  , Vector Text -- invalidApplyTx error rows
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
  , ByteString -- blockId
  , ByteString -- address
  , Int64 -- lovelace
  )

type TxOutAssetRow =
  ( ByteString -- txId
  , Int16 -- txIx
  , ByteString -- blockId
  , ByteString -- policyId
  , ByteString -- name
  , Int64 -- quantity
  )

assetsToTxOutAssetRows
  :: BlockHeader
  -> ByteString
  -> Int16
  -> Assets
  -> [TxOutAssetRow]
assetsToTxOutAssetRows BlockHeader{..} txId txIx Assets{..} =
  Map.toList (unTokens tokens) <&> \(assetId, quantity) ->
    ( txId
    , txIx
    , unBlockHeaderHash headerHash
    , unPolicyId $ policyId assetId
    , unTokenName $ tokenName assetId
    , fromIntegral quantity
    )

type ContractTxOutRow =
  ( ByteString -- txId
  , Int16 -- txIx
  , ByteString -- blockId
  , ByteString -- payoutScriptHash
  , ByteString -- contract
  , ByteString -- state
  , ByteString -- rolesCurrency
  )

transactionScriptOutputToRows
  :: BlockHeader
  -> ScriptHash
  -> TxOutRef
  -> Core.MarloweVersion v
  -> Core.TransactionScriptOutput v
  -> (ByteString, Int16, TxOutRow, ContractTxOutRow, [TxOutAssetRow])
transactionScriptOutputToRows blockHeader@BlockHeader{..} payoutValidatorHash TxOutRef{..} MarloweV1 TransactionScriptOutput{..} =
  ( txId'
  , txIx'
  , ( txId'
    , txIx'
    , unBlockHeaderHash headerHash
    , unAddress address
    , fromIntegral $ ada assets
    )
  , ( txId'
    , txIx'
    , unBlockHeaderHash headerHash
    , unScriptHash payoutValidatorHash
    , toStrict $ runPut $ put $ toDatum marloweContract
    , toStrict $ runPut $ put $ toDatum marloweState
    , fromBuiltin $ unCurrencySymbol $ rolesCurrency marloweParams
    )
  , assetsToTxOutAssetRows blockHeader txId' txIx' assets
  )
  where
    MarloweData{..} = datum
    txId' = unTxId txId
    txIx' = fromIntegral txIx

type CreateTxOutRow =
  ( ByteString -- txId
  , Int16 -- txIx
  , Int64 -- slotNo
  , ByteString -- blockId
  , Int64 -- blockNo
  , Maybe ByteString -- metadata
  )

createTxToTxOutRows :: BlockHeader -> MarloweCreateTransaction -> [(TxOutRow, ContractTxOutRow, CreateTxOutRow, [TxOutAssetRow])]
createTxToTxOutRows blockHeader@BlockHeader{..} MarloweCreateTransaction{..} =
  Map.toList newContracts <&> \(txIx, SomeCreateStep version CreateStep{..}) ->
    let
      (txId', txIx', txOutRow, contractTxOutRow, txOutAssetRows) =
        transactionScriptOutputToRows blockHeader payoutValidatorHash (TxOutRef txId txIx) version createOutput
    in
      ( txOutRow
      , contractTxOutRow
      , ( txId'
        , txIx'
        , fromIntegral slotNo
        , unBlockHeaderHash headerHash
        , fromIntegral blockNo
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
  , Int64 -- slotNo
  , ByteString -- blockId
  , Int64 -- blockNo
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
  , ByteString -- blockId
  , ByteString -- rolesCurrency
  , ByteString -- role
  )

applyTxToRows
  :: MarloweApplyInputsTransaction
  -> ( ApplyTxRow
     , Maybe (TxOutRow, ContractTxOutRow, [TxOutAssetRow])
     , [(TxOutRow, PayoutTxOutRow, [TxOutAssetRow])]
     )
applyTxToRows (MarloweApplyInputsTransaction MarloweV1 UnspentContractOutput{..} Core.Transaction{..}) =
  let
    BlockHeader{..} = blockHeader
    txId' = unTxId transactionId
    (createTxId', createTxIx') = case unContractId contractId of
      TxOutRef{..} -> (unTxId txId, fromIntegral txIx)
    (inputTxId', inputTxIx') = case txOutRef of
      TxOutRef{..} -> (unTxId txId, fromIntegral txIx)
    mOutputRows = Core.scriptOutput output <&> \scriptOutput ->
      transactionScriptOutputToRows blockHeader payoutValidatorHash (utxo scriptOutput) MarloweV1 scriptOutput
  in
    ( ( txId'
      , createTxId'
      , createTxIx'
      , fromIntegral slotNo
      , unBlockHeaderHash headerHash
      , fromIntegral blockNo
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
            , unBlockHeaderHash headerHash
            , unAddress address
            , fromIntegral $ ada assets
            )
          , ( txId'
            , txIx'
            , unBlockHeaderHash headerHash
            , unPolicyId $ policyId datum
            , unTokenName $ tokenName datum
            )
          , assetsToTxOutAssetRows blockHeader txId' txIx' assets
          )
    )

type WithdrawalTxInRow =
  ( ByteString -- txId
  , Int64 -- slotNo
  , ByteString -- blockId
  , Int64 -- blockNo
  , ByteString -- payoutTxId
  , Int16 -- payoutTxIx
  )

type InvalidApplyTxRow =
  ( ByteString -- txId
  , ByteString -- inputTxId
  , Int16 -- inputTxIx
  , ByteString -- blockId
  , Text -- error
  )

withdrawTxToWithdrawalTxInRows
  :: BlockHeader
  -> MarloweWithdrawTransaction
  -> [WithdrawalTxInRow]
withdrawTxToWithdrawalTxInRows BlockHeader{..} MarloweWithdrawTransaction{..} =
  (Map.elems consumedPayouts >>= Set.toList) <&> \TxOutRef{..} ->
    ( unTxId consumingTx
    , fromIntegral slotNo
    , unBlockHeaderHash headerHash
    , fromIntegral blockNo
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
  , V.fromList txOutBlockIdRows
  , V.fromList txOutAddressRows
  , V.fromList txOutLovelaceRows

  , V.fromList txOutAssetTxIdRows
  , V.fromList txOutAssetTxIxRows
  , V.fromList txOutAssetBlockIdRows
  , V.fromList txOutAssetPolicyIdRows
  , V.fromList txOutAssetNameRows
  , V.fromList txOutAssetQuantityRows

  , V.fromList contractTxOutTxIdRows
  , V.fromList contractTxOutTxIxRows
  , V.fromList contractTxOutBlockIdRows
  , V.fromList contractTxOutPayoutScriptHashRows
  , V.fromList contractTxOutContractRows
  , V.fromList contractTxOutStateRows
  , V.fromList contractTxOutRolesCurrencyRows

  , V.fromList createTxOutTxIdRows
  , V.fromList createTxOutTxIxRows
  , V.fromList createTxOutSlotNoRows
  , V.fromList createTxOutBlockIdRows
  , V.fromList createTxOutBlockNoRows
  , V.fromList createTxOutMetadataRows

  , V.fromList applyTxTxIdRows
  , V.fromList applyTxCreateTxIdRows
  , V.fromList applyTxCreateTxIxRows
  , V.fromList applyTxSlotNoRows
  , V.fromList applyTxBlockIdRows
  , V.fromList applyTxBlockNoRows
  , V.fromList applyTxInvalidBeforeRows
  , V.fromList applyTxInvalidHereafterRows
  , V.fromList applyTxMetadataRows
  , V.fromList applyTxInputTxIdRows
  , V.fromList applyTxInputTxIxRows
  , V.fromList applyTxInputsRows
  , V.fromList applyTxOutputTxIxRows

  , V.fromList payoutTxOutTxIdRows
  , V.fromList payoutTxOutTxIxRows
  , V.fromList payoutTxOutBlockIdRows
  , V.fromList payoutTxOutRolesCurrencyRows
  , V.fromList payoutTxOutRoleRows

  , V.fromList withdrawalTxInTxIdRows
  , V.fromList withdrawalTxInSlotNoRows
  , V.fromList withdrawalTxInBlockIdRows
  , V.fromList withdrawalTxInBlockNoRows
  , V.fromList withdrawalTxInPayoutTxIdRows
  , V.fromList withdrawalTxInPayoutTxIxRows

  , V.fromList invalidApplyTxTxIdRows
  , V.fromList invalidApplyTxInputTxIdRows
  , V.fromList invalidApplyTxInputTxIxRows
  , V.fromList invalidApplyTxBlockIdRows
  , V.fromList invalidApplyTxErrorRows
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
      , invalidApplyTxRows
      ) = foldMap8 marloweBlockToTxRows blocks

    ( txOutTxIdRows
      , txOutTxIxRows
      , txOutBlockIdRows
      , txOutAddressRows
      , txOutLovelaceRows
      ) = unzip5 txOutRows

    ( txOutAssetTxIdRows
      , txOutAssetTxIxRows
      , txOutAssetBlockIdRows
      , txOutAssetPolicyIdRows
      , txOutAssetNameRows
      , txOutAssetQuantityRows
      ) = unzip6 txOutAssetRows

    ( contractTxOutTxIdRows
      , contractTxOutTxIxRows
      , contractTxOutBlockIdRows
      , contractTxOutPayoutScriptHashRows
      , contractTxOutContractRows
      , contractTxOutStateRows
      , contractTxOutRolesCurrencyRows
      ) = unzip7 contractTxOutRows

    ( createTxOutTxIdRows
      , createTxOutTxIxRows
      , createTxOutSlotNoRows
      , createTxOutBlockIdRows
      , createTxOutBlockNoRows
      , createTxOutMetadataRows
      ) = unzip6 createTxOutRows

    ( applyTxTxIdRows
      , applyTxCreateTxIdRows
      , applyTxCreateTxIxRows
      , applyTxSlotNoRows
      , applyTxBlockIdRows
      , applyTxBlockNoRows
      , applyTxInvalidBeforeRows
      , applyTxInvalidHereafterRows
      , applyTxMetadataRows
      , applyTxInputTxIdRows
      , applyTxInputTxIxRows
      , applyTxInputsRows
      , applyTxOutputTxIxRows
      ) = unzip13 applyTxRows

    ( payoutTxOutTxIdRows
      , payoutTxOutTxIxRows
      , payoutTxOutBlockIdRows
      , payoutTxOutRolesCurrencyRows
      , payoutTxOutRoleRows
      ) = unzip5 payoutTxOutRows

    ( withdrawalTxInTxIdRows
      , withdrawalTxInSlotNoRows
      , withdrawalTxInBlockIdRows
      , withdrawalTxInBlockNoRows
      , withdrawalTxInPayoutTxIdRows
      , withdrawalTxInPayoutTxIxRows
      ) = unzip6 withdrawalTxInRows

    ( invalidApplyTxTxIdRows
      , invalidApplyTxInputTxIdRows
      , invalidApplyTxInputTxIxRows
      , invalidApplyTxBlockIdRows
      , invalidApplyTxErrorRows
      ) = unzip5 invalidApplyTxRows

marloweBlockToTxRows
  :: MarloweBlock
  -> ([TxOutRow], [TxOutAssetRow], [ContractTxOutRow], [CreateTxOutRow], [ApplyTxRow], [PayoutTxOutRow], [WithdrawalTxInRow], [InvalidApplyTxRow])
marloweBlockToTxRows MarloweBlock{..} = flip foldMap8 transactions \case
  CreateTransaction tx ->
    let
      (txOutRows, contractTxOutRows, createTxOutRows, txOutAssetRows) = unzip4 $ createTxToTxOutRows blockHeader tx
    in
      (txOutRows, concat txOutAssetRows, contractTxOutRows, createTxOutRows, [], [], [], [])
  ApplyInputsTransaction tx ->
    let
      (applyTxRow, mOutRows, payoutRows) = applyTxToRows tx
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
      , []
      )
  WithdrawTransaction tx -> ([], [], [], [] , [], [], withdrawTxToWithdrawalTxInRows blockHeader tx, [])
  InvalidApplyInputsTransaction txId txOutRefs err ->
    ( []
    , []
    , []
    , []
    , []
    , []
    , []
    , Set.toList txOutRefs <&> \(TxOutRef inputTxId inputTxIx) ->
        (unTxId txId, unTxId inputTxId, fromIntegral inputTxIx, unBlockHeaderHash $ headerHash blockHeader, T.pack $ show err)
    )
  _ -> ([], [], [], [] , [], [], [], [])


foldMap8
  :: (Monoid a, Monoid b, Monoid c, Monoid d, Monoid e, Monoid f, Monoid g, Monoid h, Foldable t)
  => (x -> (a, b, c, d, e, f, g, h))
  -> t x
  -> (a, b, c, d, e, f, g, h)
foldMap8 k = foldr
  ( \x (a', b', c', d', e', f', g', h') ->
    let
      (a, b, c, d, e, f, g, h) = k x
    in
      ( a <> a'
      , b <> b'
      , c <> c'
      , d <> d'
      , e <> e'
      , f <> f'
      , g <> g'
      , h <> h'
      )
  )
  (mempty, mempty, mempty, mempty, mempty, mempty, mempty, mempty)

unzip13 :: [(a, b, c, d, e, f, g, h, i, j, k, l, m)] -> ([a], [b], [c], [d], [e], [f], [g], [h], [i], [j], [k], [l], [m])
unzip13 = foldr
  (\(a, b, c, d, e, f, g, h, i, j, k, l, m) (as, bs, cs, ds, es, fs, gs, hs, is, js, ks, ls, ms) ->
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
    , l : ls
    , m : ms
    )
  )
  ([], [], [], [], [], [], [], [], [], [], [], [], [])
