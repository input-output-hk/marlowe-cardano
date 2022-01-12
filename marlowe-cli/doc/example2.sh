#!/usr/bin/env bash

set -e

CARDANO_NODE_SOCKET_PATH=$(pgrep cardano-wallet | grep testnet | sed -E 's/(.*)node-socket //')
export CARDANO_NODE_SOCKET_PATH
TESTNET_MAGIC=1097911063
export TESTNET_MAGIC

generate_address() {
  cardano-cli query protocol-parameters --testnet-magic 1097911063 > testnet-params.json

  cardano-cli address key-gen --verification-key-file payment.vkey --signing-key-file payment.skey

  cardano-cli stake-address key-gen --verification-key-file stake.vkey --signing-key-file stake.skey

  cardano-cli address build --payment-verification-key-file payment.vkey --stake-verification-key-file stake.vkey \
  --out-file payment.addr --testnet-magic 1097911063
}

NETWORK=testnet
MAGIC="--testnet-magic 1097911063"

cardano-cli query protocol-parameters "$MAGIC" --out-file $NETWORK.protocol
CUR_SLOT=$(cardano-cli get-tip "$MAGIC" | jq -r '.slot')

echo "Current slot ${CUR_SLOT}"

# PAYMENT_SKEY=payment.skey
# PAYMENT_VKEY=payment.vkey
ADDRESS_P=$(cat payment.addr)
# PUBKEYHASH_P=$(cardano-cli address key-hash --payment-verification-key-file $PAYMENT_VKEY)
CONTRACT_FILE=c1.contract
STATE_FILE=c1.state
DATUM_LOVELACE=$(jq '.accounts | .[0] | .[1]' $STATE_FILE)
# DATUM_MIN_SLOT=10
REDEEMER_MIN_SLOT=44316000
REDEEMER_MAX_SLOT=44317000 #$((CUR_SLOT+1000))


ADDRESS_S=$(marlowe-cli contract address --testnet-magic 1097911063)

PLUTUS_FILE=test.plutus
DATUM_FILE=test.datum
REDEEMER_FILE=test.redeemer

DATUM_HASH=$(
marlowe-cli contract datum --contract-file $CONTRACT_FILE \
                  --state-file $STATE_FILE       \
                  --out-file $DATUM_FILE
)

marlowe-cli contract validator "$MAGIC" --out-file $PLUTUS_FILE
# marlowe-cli contract redeemer --min-slot $REDEEMER_MIN_SLOT \
#                      --max-slot $REDEEMER_MAX_SLOT \
#                      --out-file $REDEEMER_FILE


COLLATERAL_TX=e0ec3e100fa03dcbcc02ca3245b5770659065528642845c1d1d572401548b952#1

# Create the contract, and extract the address, validator, datum hash, datum, and redeemer.
create_contract_tx() {
  # Find funds, and enter the selected UTxO as "TX_0".


  TX_0=$(cardano-cli query utxo "$MAGIC" --address "$ADDRESS_P" --out-file /dev/stdout | jq -r 'keys | .[0]')


  # # Fund the contract.

  cardano-cli transaction build --alonzo-era "$MAGIC"                 \
                                --tx-in "$TX_0"                    \
                                --tx-out "$ADDRESS_S"+"$DATUM_LOVELACE" \
                                  --tx-out-datum-hash "$DATUM_HASH"   \
                                --change-address "$ADDRESS_P"         \
                                --out-file tx.raw
  cardano-cli transaction view --tx-body-file tx.raw
}

sign_submit() {
  cardano-cli transaction sign "$MAGIC"                          \
                              --tx-body-file tx.raw           \
                              --signing-key-file payment.skey \
                              --out-file tx.signed

  cardano-cli transaction submit "$MAGIC" --tx-file tx.signed
}

redeem() {
  FEE_UTXO=19456bb108c0fa182dd7ab11c6ef927cbe36d6ebc743f5099a6209e26d8862f0#0
  CONTRACT_UTXO=19456bb108c0fa182dd7ab11c6ef927cbe36d6ebc743f5099a6209e26d8862f0#1

  FUNDS=$(cardano-cli query utxo "$MAGIC" --address "$ADDRESS_P" --out-file /dev/stdout | jq '.["'$FEE_UTXO'"].value.lovelace')

  FEE=$(
  cardano-cli transaction build --alonzo-era "$MAGIC"                      \
                                --protocol-params-file $NETWORK.protocol \
                                --tx-in $FEE_UTXO                         \
                                --tx-in $CONTRACT_UTXO                          \
                                --tx-in-script-file $PLUTUS_FILE       \
                                --tx-in-datum-file $DATUM_FILE         \
                                --tx-in-redeemer-file $REDEEMER_FILE \
                                --tx-out "$ADDRESS_P"+3000000              \
                                --change-address "$ADDRESS_P"              \
                                --tx-in-collateral $COLLATERAL_TX               \
                                --invalid-before $REDEEMER_MIN_SLOT      \
                                --invalid-hereafter $REDEEMER_MAX_SLOT   \
                                --out-file tx.raw                        \
  | sed -e 's/^.* //'
  )

  NET=$((DATUM_LOVELACE + FUNDS - FEE))

  echo "$DATUM_LOVELACE + $FUNDS - $FEE = $NET"

  cardano-cli transaction build  \
    --alonzo-era \
    --testnet-magic ${TESTNET_MAGIC} \
    --invalid-before $REDEEMER_MIN_SLOT      \
    --invalid-hereafter $REDEEMER_MAX_SLOT   \
    --tx-in $FEE_UTXO \
    --tx-in $CONTRACT_UTXO \
    --tx-in-script-file $PLUTUS_FILE  \
    --tx-in-datum-file $DATUM_FILE \
    --tx-in-redeemer-file $REDEEMER_FILE \
    --tx-in-collateral $COLLATERAL_TX \
    --tx-out "$ADDRESS_P"+$NET \
    --change-address "$ADDRESS_P" \
    --protocol-params-file $NETWORK.protocol \
    --out-file tx.raw
  cardano-cli transaction view --tx-body-file tx.raw
}

# cardano-cli transaction sign "$MAGIC"                          \
#                              --tx-body-file tx.raw           \
#                              --signing-key-file payment.skey \
#                              --out-file tx.signed

# cardano-cli transaction submit "$MAGIC" --tx-file tx.signed


# # See that the transaction succeeded.

# create_contract_tx
redeem
sign_submit

cardano-cli query utxo "$MAGIC" --address "$ADDRESS_S"
cardano-cli query utxo "$MAGIC" --address "$ADDRESS_P"


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
