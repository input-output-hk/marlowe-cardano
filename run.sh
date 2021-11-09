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

# Marlowe output
cardano-cli transaction build \
--alonzo-era \
--testnet-magic ${TESTNET_MAGIC} \
--change-address $(cat payment.addr) \
--tx-in e8c2689665dbc44f815e2a315f674a2cf7f88250bf3182cd1021724c487d019f#0 \
--tx-out addr_test1wpq55y4zw6sjplwjh03m8h30chv2wyzwk84pwewv2l8djqcfzvpa0+3000000 \
--tx-out-datum-hash 6b80f9cad44433d6e5692ff389a8899a5d8cd6bb0485136a97f86e98eb850510 \
--protocol-params-file testnet-params.json \
--metadata-json-file metadata.json \
--out-file tx.build

cardano-cli query utxo --testnet-magic $TESTNET_MAGIC --address $(cat payment.addr)
cardano-cli query utxo --testnet-magic $TESTNET_MAGIC --address addr_test1wpq55y4zw6sjplwjh03m8h30chv2wyzwk84pwewv2l8djqcfzvpa0

# Spend

cardano-cli transaction build \
--alonzo-era \
--testnet-magic ${TESTNET_MAGIC} \
--invalid-before 5 \
--invalid-hereafter 43500000 \
--tx-in 09f960e983782278ab5b97b357b6cf71c278199d1e027d62281cd9f648bbb728#0 \
--tx-in 09f960e983782278ab5b97b357b6cf71c278199d1e027d62281cd9f648bbb728#1 \
--tx-in-script-file script.json  \
--tx-in-datum-file datum.json \
--tx-in-redeemer-file redeemer.json \
--tx-in-collateral e0ec3e100fa03dcbcc02ca3245b5770659065528642845c1d1d572401548b952#1 \
--tx-out $(cat payment.addr)+3000000 \
--change-address $(cat payment.addr) \
--protocol-params-file testnet-params.json \
--metadata-json-file metadata.json \
--out-file marlowe-close.tx

cardano-cli transaction sign \
--tx-body-file marlowe-close.tx \
--testnet-magic ${TESTNET_MAGIC} \
--signing-key-file payment.skey \
--out-file marlowe-close.signed

cardano-cli transaction submit --tx-file marlowe-close.signed --testnet-magic ${TESTNET_MAGIC}


cardano-cli query utxo --testnet-magic $TESTNET_MAGIC --address $(cat payment.addr)
cardano-cli query utxo --testnet-magic $TESTNET_MAGIC --address addr_test1wrgve7kfzxsqczwt8g6a4paus0tssgk4x9vuv8cc2shwq3s5cqts4

cardano-cli transaction view --tx-body-file marlowe-close.tx

## MaxTxSizeUTxO 18055 16384
# Size: 17621
# Size: 17522 fa29409229673ff4ae1f4de0374867e77f73a4ab3deebe25271f8275 -- reduces error strings
# Size: 16793 484c4125ac40d2a47dfb9d6c09ef4c25f06df779284465cd6837696b -- remove constraints
# Default Marlowe validator size: 18137 -> 18053 (eval Scale -84) -> 18053 (Eq Scale)

# Simplified Marlowe: 16124 -> 16557
# 15629 (no TxConstraints, no check output) 15983 (no check output), 17239 (with check output)
# 17122
# 15014 no TxConstraints, check everything
# 9776 no computeTransaction
# 14870 no TxConstraints, check everything, timeToSlot
# 14854 OutputConstraints, check everything, timeToSlot
# 14835 OutputConstraints, check everything, timeToSlot, redeemer [Input]
# 14917 OutputConstraints, check everything, timeToSlot, redeemer [Input], with Scale

## First successfull spending addr_test1wpfxda7xxcx7w3dmtqrahx2fgp6nspll5putm7qpw6dz2vs0tgayn

# Marlowe Pay
cardano-cli transaction build \
--alonzo-era \
--testnet-magic ${TESTNET_MAGIC} \
--change-address $(cat payment.addr) \
--tx-in 4a4d081947ecf7194310c118d0a68a442ee4a4c970e058fb109db066fd859387#0 \
--tx-in c72b82b88f4de9ab100dc522ce9190064263f2a2d8cbe9e0802d530031825733#0 \
--tx-out addr_test1wrgve7kfzxsqczwt8g6a4paus0tssgk4x9vuv8cc2shwq3s5cqts4+3000000 \
--tx-out-datum-hash aa83f44809d63a3caa06f9d11d0349a637ad6dd85974e4cabb79cad866070aba \
--protocol-params-file testnet-params.json \
--metadata-json-file metadata.json \
--out-file tx.build

cardano-cli transaction sign --tx-body-file tx.build --testnet-magic ${TESTNET_MAGIC} --signing-key-file payment.skey --out-file tx.signed
cardano-cli transaction submit --tx-file tx.signed --testnet-magic ${TESTNET_MAGIC}


# Spend

cardano-cli transaction build \
--alonzo-era \
--testnet-magic ${TESTNET_MAGIC} \
--invalid-before 42127010 \
--invalid-hereafter 42128000 \
--tx-in e8c2689665dbc44f815e2a315f674a2cf7f88250bf3182cd1021724c487d019f#0 \
--tx-in e8c2689665dbc44f815e2a315f674a2cf7f88250bf3182cd1021724c487d019f#1 \
--tx-in-script-file script.json  \
--tx-in-datum-file datum-deposit.json \
--tx-in-redeemer-file redeemer.json \
--tx-in-collateral e0ec3e100fa03dcbcc02ca3245b5770659065528642845c1d1d572401548b952#1 \
--tx-out $(cat payment.addr)+3000000 \
--change-address $(cat payment.addr) \
--protocol-params-file testnet-params.json \
--metadata-json-file metadata.json \
--out-file marlowe-close.tx

cardano-cli transaction sign \
--tx-body-file marlowe-close.tx \
--testnet-magic ${TESTNET_MAGIC} \
--signing-key-file payment.skey \
--out-file marlowe-close.signed
