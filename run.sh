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
cardano-cli query utxo --testnet-magic $TESTNET_MAGIC --address addr_test1wr7mml52pdlnmg2s4344ln4yx2nwvrg9utqtggqa0g0z6ccceyt9x

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

# 7897 without computeTransaction => 14959 - 7897 = 7062 for Marlowe interpreter stuff

## First successfull spending addr_test1wpfxda7xxcx7w3dmtqrahx2fgp6nspll5putm7qpw6dz2vs0tgayn

cardano-cli get-tip --testnet-magic 1097911063

saddr="addr_test1wqkvngwt2lwt6cdp74t6k3zzah3m8h6w3ctmcyvwufux3ws82q2e6"

# Marlowe Pay
cardano-cli transaction build \
--alonzo-era \
--testnet-magic ${TESTNET_MAGIC} \
--change-address $(cat payment.addr) \
--tx-in 6304f892d594a06de7ffd005c8d74b5dca83f1d6aa194407b6f727d3fe0b6518#0 \
--tx-out ${saddr}+3000000 \
--tx-out-datum-hash d3829ca97bf76335473c2846e7152fef58f56745e9813fea2820d7535bef868b \
--protocol-params-file testnet-params.json \
--metadata-json-file metadata.json \
--out-file tx.build

cardano-cli transaction sign --tx-body-file tx.build --testnet-magic ${TESTNET_MAGIC} --signing-key-file payment.skey --out-file tx.signed
cardano-cli transaction submit --tx-file tx.signed --testnet-magic ${TESTNET_MAGIC}

cardano-cli query utxo --testnet-magic $TESTNET_MAGIC --address $(cat payment.addr)
cardano-cli query utxo --testnet-magic $TESTNET_MAGIC --address ${saddr}

cardano-cli transaction view --tx-body-file marlowe-close.tx

# Spend

cardano-cli transaction build \
--alonzo-era \
--testnet-magic ${TESTNET_MAGIC} \
--invalid-before 42292000 \
--invalid-hereafter 42294000 \
--tx-in 07f7fa34d5fc038d6fc3465074a439abb6f77bfd1969ad64a8b5580f5959da5f#0 \
--tx-in 07f7fa34d5fc038d6fc3465074a439abb6f77bfd1969ad64a8b5580f5959da5f#1 \
--tx-in-script-file script.json  \
--tx-in-datum-file datum-deposit.json \
--tx-in-redeemer-file redeemer-deposit.json \
--tx-in-collateral e0ec3e100fa03dcbcc02ca3245b5770659065528642845c1d1d572401548b952#1 \
--tx-out $(cat payment.addr)+12000000 \
--change-address $(cat payment.addr) \
--protocol-params-file testnet-params.json \
--metadata-json-file metadata.json \
--out-file marlowe-close.tx

cardano-cli transaction sign \
--tx-body-file marlowe-close.tx \
--testnet-magic ${TESTNET_MAGIC} \
--signing-key-file payment.skey \
--out-file marlowe-close.signed
