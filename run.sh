#!/bin/bash

echo 'Test'

export CARDANO_NODE_SOCKET_PATH=$(ps ax | grep -v grep | grep cardano-wallet | grep testnet | sed -E 's/(.*)node-socket //')

# Check var it must be path for file of node socket and not empty
echo $CARDANO_NODE_SOCKET_PATH
export TESTNET_MAGIC=1097911063
# Check connect if yor run Daedalus for Testnet
cardano-cli get-tip --testnet-magic 1097911063

cardano-cli query protocol-parameters --testnet-magic 1097911063 > testnet-params.json

cardano-cli address key-gen --verification-key-file payment.vkey --signing-key-file payment.skey

cardano-cli stake-address key-gen --verification-key-file stake.vkey --signing-key-file stake.skey

cardano-cli address build --payment-verification-key-file payment.vkey --stake-verification-key-file stake.vkey \
 --out-file payment.addr --testnet-magic 1097911063


# Marlowe Validator hash e66ca5140db1c3e4ddcb88f68ab7f6c622ae48ff0b8448ad819d5785

cardano-cli transaction build \
--alonzo-era \
--testnet-magic ${TESTNET_MAGIC} \
--tx-in 9931a9fb57b260a56c204528554560a6cf26faf2197bc41110fce21469e204fa#0 \
--tx-out $(cat payment.addr)+5000000 \
--change-address $(cat payment.addr) \
--out-file tx.build

cardano-cli transaction sign --tx-body-file tx.build --testnet-magic ${TESTNET_MAGIC} --signing-key-file payment.skey --out-file tx.signed
cardano-cli transaction submit --tx-file tx.signed --testnet-magic ${TESTNET_MAGIC}


cardano-cli transaction build \
--alonzo-era \
--testnet-magic ${TESTNET_MAGIC} \
--change-address $(cat payment.addr) \
--tx-in 41a2d07e1eea34c17b0bda443512c4c8c82d836570f4133b749b6f0ac37f5f8f#0 \
--tx-in 6bc05f7dc3a7f8eae64ee53ffc676005c30fa81276452daa411e9942484456ac#0 \
--tx-out addr_test1wqqwzc3j8jrqfn0scn8ydm5mesla5xrg3mqpcmp8hjplt6q2ng95l+3000000 \
--tx-out-datum-hash 6b3d95fbfa8421475d74523bca4d9c433194812ae37ffb2a87ea2cb7cef2e99e \
--protocol-params-file testnet-params.json \
--metadata-json-file metadata.json \
--out-file tx.build


# no constraints
cardano-cli transaction build \
--alonzo-era \
--testnet-magic ${TESTNET_MAGIC} \
--change-address $(cat payment.addr) \
--tx-in 7e9951227de6875bafc1ca3f533068ceed31e322a5668f74913213f5d909ab44#0 \
--tx-out addr_test1wqqwzc3j8jrqfn0scn8ydm5mesla5xrg3mqpcmp8hjplt6q2ng95l+3000000 \
--tx-out-datum-hash 6b80f9cad44433d6e5692ff389a8899a5d8cd6bb0485136a97f86e98eb850510 \
--protocol-params-file testnet-params.json \
--metadata-json-file metadata.json \
--out-file tx.build

# Spend

cardano-cli transaction build \
--alonzo-era \
--testnet-magic ${TESTNET_MAGIC} \
--invalid-before 10000 \
--invalid-hereafter 43000000 \
--tx-in f19f90cd1b0a55606af579c13547c1770ce0a3fc950f1be2ba04ed4f249f82f1#0 \
--tx-in f19f90cd1b0a55606af579c13547c1770ce0a3fc950f1be2ba04ed4f249f82f1#1 \
--tx-in-script-file script.json  \
--tx-in-datum-file datum.json \
--tx-in-redeemer-file redeemer.json \
--tx-in-collateral e0ec3e100fa03dcbcc02ca3245b5770659065528642845c1d1d572401548b952#1 \
--tx-out $(cat payment.addr)+69955230 \
--change-address $(cat payment.addr) \
--protocol-params-file testnet-params.json \
--out-file marlowe-close.tx

cardano-cli transaction sign \
--tx-body-file marlowe-close.tx \
--testnet-magic ${TESTNET_MAGIC} \
--signing-key-file payment.skey \
--out-file marlowe-close.signed

cardano-cli transaction submit --tx-file marlowe-close.signed --testnet-magic ${TESTNET_MAGIC}


cardano-cli query utxo --address $(cat payment.addr) --testnet-magic $TESTNET_MAGIC
cardano-cli query utxo --address addr_test1wrmw97nzdww7a035ny88zmpcswtxdmud6a4cndzt6l7zz8cyky7r9 --testnet-magic $TESTNET_MAGIC

cardano-cli transaction view --tx-body-file marlowe-close.tx

## MaxTxSizeUTxO 18055 16384
# Size: 17621
# Size: 17522 fa29409229673ff4ae1f4de0374867e77f73a4ab3deebe25271f8275 -- reduces error strings
# Size: 16793 484c4125ac40d2a47dfb9d6c09ef4c25f06df779284465cd6837696b -- remove constraints

## First successfull spending addr_test1wpfxda7xxcx7w3dmtqrahx2fgp6nspll5putm7qpw6dz2vs0tgayn
