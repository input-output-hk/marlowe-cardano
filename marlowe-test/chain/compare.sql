--
-- Script to compare databases for `marlowe-chainindexer` and `cardano-db-sync`.
--
-- This assumes that the `marlowe-chainindexer` tables reside in the schema
-- `chain` and the `cardano-db-sync` tables reside in the schema `dbsync`.
--
-- This script writes CSV output files listing discrepancies to a folder `out/`.
--


\qecho
\qecho Finding the last block in common.
\qecho

drop table if exists max_slotno;
create temporary table max_slotno as
  select
      max(slotno) as max_slotno
    from chain.block as cblock
    inner join dbsync.block as dblock
      on cblock.id = dblock.hash
;
select max_slotno as "Latest Block in Common"
  from max_slotno
;
\copy max_slotno to 'out/latest-block.csv' CSV HEADER


\qecho
\qecho Block comparison.
\qecho

drop table if exists cmp_block;
create temporary table cmp_block as
  select
      'chainindex' :: varchar as "source"
    , id
    , slotno
    , blockno
    from max_slotno
    inner join chain.block
      on slotno <= max_slotno
    where slotno >= 0
  union all
  select
      'dbsync'
    , hash
    , slot_no
    , block_no
    from max_slotno
    inner join dbsync.block
      on slot_no <= max_slotno
;
select
    source as "Source"
  , count(*) as "Count of Block Records"
  from cmp_block
  group by source
;

drop table if exists x_block;
create temporary table x_block as
  select 'chainindex-dbsync' :: varchar as "comparison", *
    from (
      select id, slotno, blockno from cmp_block where source = 'chainindex'
      except
      select id, slotno, blockno from cmp_block where source = 'dbsync'
    ) as mc_block
  union all
  select 'dbsync-chainindex', *
    from (
      select id, slotno, blockno from cmp_block where source = 'dbsync'
      except
      select id, slotno, blockno from cmp_block where source = 'chainindex'
    ) as cm_block
;
select
    comparison as "Discrepancy"
  , count(*) as "Count of Block Records"
  from x_block
  group by comparison
;
\copy (select * from x_block order by 2, 3, 4, 1) to 'out/block-discrepancies.csv' CSV HEADER


\qecho
\qecho Tx comparison.
\qecho

drop table if exists cmp_tx;
create temporary table cmp_tx as
  select
      'chainindex' :: varchar as "source"
    , id
    , blockid
    , slotno
    , validitylowerbound
    , validityupperbound
    , metadata
    , isvalid
    from max_slotno
    inner join chain.tx
      on slotno <= max_slotno
    where slotno >= 0
  union all
  select
      'dbsync'
    , tx.hash
    , block.hash
    , block.slot_no
    , invalid_before
    , invalid_hereafter
    , tx_metadata.bytes
    , valid_contract
    from max_slotno
    inner join dbsync.block
      on block.slot_no <= max_slotno
    inner join dbsync.tx
      on tx.block_id = block.id
    left outer join dbsync.tx_metadata
      on tx.id = tx_metadata.tx_id
;
select
    source as "Source"
  , count(*) as "Count of Tx Records"
  from
  cmp_tx
  group by source
;

drop table if exists x_tx;
create temporary table x_tx as
  select 'chainindex-dbsync' :: varchar as "comparison", *
    from (
      select id, blockid, slotno, validitylowerbound, validityupperbound, isvalid from cmp_tx where source = 'chainindex'
      except
      select id, blockid, slotno, validitylowerbound, validityupperbound, isvalid from cmp_tx where source = 'dbsync'
    ) as mc_tx
  union all
  select 'dbsync-chainindex', *
    from (
      select id, blockid, slotno, validitylowerbound, validityupperbound, isvalid from cmp_tx where source = 'dbsync'
      except
      select id, blockid, slotno, validitylowerbound, validityupperbound, isvalid from cmp_tx where source = 'chainindex'
    ) as cm_tx
;
select
    comparison as "Discrepancy"
  , count(*) as "Count of Tx Records"
  from x_tx
  group by comparison
;
\copy (select * from x_tx order by 2, 3, 4, 5, 6, 7, 1) to 'out/tx-discrepancies.csv' CSV HEADER

-- FIXME: Add metadata substring find.


\qecho
\qecho TxOut comparison.
\qecho

drop table if exists cmp_txout;
create temporary table cmp_txout as
  select
      'chainindex' :: varchar as "source"
    , txid
    , txix
    , slotno
    , address
    , lovelace
    , datumhash
    , datumbytes
    , iscollateral
    from max_slotno
    inner join chain.txout
      on slotno <= max_slotno
    where slotno >= 0
  union all
  select
      'dbsync'
    , tx.hash
    , tx_out.index
    , block.slot_no
    , tx_out.address_raw
    , tx_out.value
    , tx_out.data_hash
    , datum.bytes
    , not tx.valid_contract
    from max_slotno
    inner join dbsync.block
      on block.slot_no <= max_slotno
    inner join dbsync.tx
      on tx.block_id = block.id
    inner join dbsync.tx_out
      on tx_out.tx_id = tx.id
    left outer join dbsync.datum
      on datum.hash = tx_out.data_hash
;
select
    source as "Source"
  , count(*) as "Count of TxOut Records"
  from cmp_txout
  group by source
;

drop table if exists x_txout;
create temporary table x_txout as
  select 'chainindex-dbsync' :: varchar as "comparison", *
    from (
      select txid, txix, slotno, address, lovelace, datumhash, datumbytes, iscollateral from cmp_txout where source = 'chainindex'
      except
      select txid, txix, slotno, address, lovelace, datumhash, datumbytes, iscollateral from cmp_txout where source = 'dbsync'
    ) as mc_txout
  union all
  select 'dbsync-chainindex', *
    from (
      select txid, txix, slotno, address, lovelace, datumhash, datumbytes, iscollateral from cmp_txout where source = 'dbsync'
      except
      select txid, txix, slotno, address, lovelace, datumhash, datumbytes, iscollateral from cmp_txout where source = 'chainindex'
    ) as cm_txout
;
select
    comparison as "Discrepancy"
  , count(*) as "Count of TxOut Records"
  from x_txout
  group by comparison
;
\copy (select * from x_txout order by 2, 3, 4, 5, 6, 7, 8, 9, 1) to 'out/txout-discrepancies.csv' CSV HEADER


\qecho
\qecho TxIn comparison.
\qecho

drop table if exists cmp_txin;
create temporary table cmp_txin as
  select
      'chainindex' :: varchar as "source"
    , txoutid
    , txoutix
    , txinid
    , slotno
    , redeemerdatumbytes
    , iscollateral
    from max_slotno
    inner join chain.txin
      on slotno <= max_slotno
    where slotno >= 0
  union all
  select
      'dbsync'
    , out_tx.hash
    , tx_out_index
    , in_tx.hash
    , block.slot_no
    , redeemer_data.bytes
    , not in_tx.valid_contract
    from max_slotno
    inner join dbsync.block as block
      on block.slot_no <= max_slotno
    inner join dbsync.tx as in_tx
      on in_tx.block_id = block.id
    inner join dbsync.tx_in
      on tx_in.tx_in_id = in_tx.id
    inner join dbsync.tx_out
      on tx_out.tx_id = tx_in.tx_out_id and tx_out.index = tx_in.tx_out_index
    inner join dbsync.tx as out_tx
      on out_tx.id = tx_out.tx_id
    left outer join dbsync.redeemer
      on redeemer.id = tx_in.redeemer_id
    left outer join dbsync.redeemer_data
      on redeemer_data.id = redeemer.redeemer_data_id
;
select
    source as "Source"
  , count(*) as "Count of TxIn Records"
  from cmp_txin
  group by source
;

drop table if exists x_txin;
create temporary table x_txin as
  select 'chainindex-dbsync' :: varchar as "comparison", *
  from (
    select txoutid, txoutix, txinid, slotno, redeemerdatumbytes from cmp_txin where source = 'chainindex'
    except
    select txoutid, txoutix, txinid, slotno, redeemerdatumbytes from cmp_txin where source = 'dbsync'
  ) as mc_txin
  union all
  select 'dbsync-chainindex', *
  from (
    select txoutid, txoutix, txinid, slotno, redeemerdatumbytes from cmp_txin where source = 'dbsync'
    except
    select txoutid, txoutix, txinid, slotno, redeemerdatumbytes from cmp_txin where source = 'chainindex'
  ) as cm_txin
;
select
    comparison as "Discrepancy"
  , count(*) as "Count of TxIn Records"
  from x_txin
  group by comparison
;
\copy (select * from x_txin order by 2, 3, 4, 5, 6, 1) to 'out/txin-discrepancies.csv' CSV HEADER


\qecho
\qecho Asset comparison.
\qecho

drop table if exists cmp_asset;
create temporary table cmp_asset as
  select
      'chainindex' :: varchar as "source"
    , policyid
    , name
    from max_slotno
    inner join chain.assetmint
      on slotno <= max_slotno
    inner join chain.asset
      on assetid = id
  union  -- NB: We need `union` instead of `union all` here because assets may be minted more than once.
  select
      'dbsync'
    , multi_asset.policy
    , multi_asset.name
    from max_slotno
    inner join dbsync.block
      on block.slot_no <= max_slotno
    inner join dbsync.tx
      on tx.block_id = block.id
    inner join dbsync.ma_tx_mint
      on ma_tx_mint.tx_id = tx.id
    inner join dbsync.multi_asset
      on multi_asset.id = ident
;
select
    source as "Source"
  , count(*) as "Count of Asset Records"
  from cmp_asset
  group by source
;

drop table if exists x_asset;
create temporary table x_asset as
  select 'chainindex-dbsync' :: varchar as "comparison", *
    from (
      select policyid, name from cmp_asset where source = 'chainindex'
      except
      select policyid, name from cmp_asset where source = 'dbsync'
    ) as mc_asset
  union all
  select 'dbsync-chainindex', *
    from (
      select policyid, name from cmp_asset where source = 'dbsync'
      except
      select policyid, name from cmp_asset where source = 'chainindex'
    ) as cm_asset
;
select
    comparison as "Discrepancy"
  , count(*) as "Count of Asset Records"
  from x_asset
  group by comparison
;
\copy (select * from x_asset order by 2, 3, 1) to 'out/asset-discrepancies.csv' CSV HEADER


\qecho
\qecho Asset mint comparison.
\qecho

drop table if exists cmp_asset_mint;
create temporary table cmp_asset_mint as
  select
      'chainindex' :: varchar as "source"
    , txid
    , slotno
    , policyid
    , name
    , quantity
    from max_slotno
    inner join chain.assetmint
      on slotno <= max_slotno
    inner join chain.asset
      on assetid = id
  union
  select
      'dbsync'
    , tx.hash
    , block.slot_no
    , multi_asset.policy
    , multi_asset.name
    , ma_tx_mint.quantity
    from max_slotno
    inner join dbsync.block
      on block.slot_no <= max_slotno
    inner join dbsync.tx
      on tx.block_id = block.id
    inner join dbsync.ma_tx_mint
      on ma_tx_mint.tx_id = tx.id
    inner join dbsync.multi_asset
      on multi_asset.id = ident
;
select
    source as "Source"
  , count(*) as "Count of Asset Mint Records"
  from cmp_asset_mint
  group by source
;

drop table if exists x_asset_mint;
create temporary table x_asset_mint as
  select 'chainindex-dbsync' :: varchar as "comparison", *
    from (
      select txid, slotno, policyid, name, quantity from cmp_asset_mint where source = 'chainindex'
      except
      select txid, slotno, policyid, name, quantity from cmp_asset_mint where source = 'dbsync'
    ) as mc_asset_mint
  union all
  select 'dbsync-chainindex', *
    from (
      select txid, slotno, policyid, name, quantity from cmp_asset_mint where source = 'dbsync'
      except
      select txid, slotno, policyid, name, quantity from cmp_asset_mint where source = 'chainindex'
    ) as cm_asset_mint
;
select
    comparison as "Discrepancy"
  , count(*) as "Count of Asset Mint Records"
  from x_asset_mint
  group by comparison
;
\copy (select * from x_asset_mint order by 2, 3, 4, 5, 6, 1) to 'out/assetmint-discrepancies.csv' CSV HEADER


\qecho
\qecho Asset out comparison.
\qecho

drop table if exists cmp_asset_out;
create temporary table cmp_asset_out as
  select
      'chainindex' :: varchar as "source"
    , txoutid
    , txoutix
    , slotno
    , policyid
    , name
    , quantity
    from max_slotno
    inner join chain.assetout
      on slotno <= max_slotno
    inner join chain.asset
      on assetid = id
  union
  select
      'dbsync'
    , tx.hash
    , tx_out.index
    , block.slot_no
    , multi_asset.policy
    , multi_asset.name
    , ma_tx_out.quantity
    from max_slotno
    inner join dbsync.block as block
      on block.slot_no <= max_slotno
    inner join dbsync.tx as tx
      on tx.block_id = block.id
    inner join dbsync.tx_out
      on tx_out.tx_id = tx.id
    inner join dbsync.ma_tx_out
      on ma_tx_out.tx_out_id = tx_out.id
    inner join dbsync.multi_asset
      on multi_asset.id = ident
;
select
    source as "Source"
  , count(*) as "Count of Asset Out Records"
  from cmp_asset_out
  group by source
;

drop table if exists x_asset_out;
create temporary table x_asset_out as
  select 'chainindex-dbsync' as "comparison", *
    from (
      select txoutid, txoutix, slotno, policyid, name, quantity from cmp_asset_out where source = 'chainindex'
      except
      select txoutid, txoutix, slotno, policyid, name, quantity from cmp_asset_out where source = 'dbsync'
    ) as mc_asset
  union all
  select 'dbsync-chainindex', *
    from (
      select txoutid, txoutix, slotno, policyid, name, quantity from cmp_asset_out where source = 'dbsync'
      except
      select txoutid, txoutix, slotno, policyid, name, quantity from cmp_asset_out where source = 'chainindex'
    ) as cm_asset
;
select
    comparison as "Discrepancy"
  , count(*) as "Count of Asset Out Records"
  from x_asset_out
  group by comparison
;
\copy (select * from x_asset_out order by 2, 3, 4, 5, 6, 7, 1) to 'out/assetout-discrepancies.csv' CSV HEADER


\qecho
\qecho Summary
\qecho

drop table if exists x_summary;
create table x_summary as
  select
      'block' :: varchar as "Table"
    , (select count(*) from x_block where comparison = 'chainindex-dbsync') as "Count of ChainIndex Minus DbSync"
    , (select count(*) from x_block where comparison = 'dbsync-chainindex') as "Count of DbSync Minus ChainIndex"
    , (select count(*) from x_block) > 0 as "Discrepancies?"
  union all
  select
      'tx'
    , (select count(*) from x_tx where comparison = 'chainindex-dbsync') as "Count of ChainIndex Minus DbSync"
    , (select count(*) from x_tx where comparison = 'dbsync-chainindex') as "Count of DbSync Minus ChainIndex"
    , (select count(*) from x_tx) > 0 as "Discrepancies?"
  union all
  select
      'txout'
    , (select count(*) from x_txout where comparison = 'chainindex-dbsync') as "Count of ChainIndex Minus DbSync"
    , (select count(*) from x_txout where comparison = 'dbsync-chainindex') as "Count of DbSync Minus ChainIndex"
    , (select count(*) from x_txout) > 0 as "Discrepancies?"
  union all
  select
      'txin'
    , (select count(*) from x_txin where comparison = 'chainindex-dbsync') as "Count of ChainIndex Minus DbSync"
    , (select count(*) from x_txin where comparison = 'dbsync-chainindex') as "Count of DbSync Minus ChainIndex"
    , (select count(*) from x_txin) > 0 as "Discrepancies?"
  union all
  select
      'asset'
    , (select count(*) from x_asset where comparison = 'chainindex-dbsync') as "Count of ChainIndex Minus DbSync"
    , (select count(*) from x_asset where comparison = 'dbsync-chainindex') as "Count of DbSync Minus ChainIndex"
    , (select count(*) from x_asset) > 0 as "Discrepancies?"
  union all
  select
      'assetmint'
    , (select count(*) from x_asset_mint where comparison = 'chainindex-dbsync') as "Count of ChainIndex Minus DbSync"
    , (select count(*) from x_asset_mint where comparison = 'dbsync-chainindex') as "Count of DbSync Minus ChainIndex"
    , (select count(*) from x_asset_mint) > 0 as "Discrepancies?"
  union all
  select
      'assetout'
    , (select count(*) from x_asset_out where comparison = 'chainindex-dbsync') as "Count of ChainIndex Minus DbSync"
    , (select count(*) from x_asset_out where comparison = 'dbsync-chainindex') as "Count of DbSync Minus ChainIndex"
    , (select count(*) from x_asset_out) > 0 as "Discrepancies?"
;
select *
  from x_summary
;

select bool_or("Discrepancies?") as "Any Failures?"
  from x_summary
;
