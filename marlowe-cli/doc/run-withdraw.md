# Marlowe CLI: Withdraw Funds from a Role Address

The `marlowe-cli run withdraw` command creates and optionally runs a transaction to withdraw funds from a Marlowe role address.


## Usage

    $ marlowe-cli run withdraw --help
    
    Usage: marlowe-cli run withdraw [--testnet-magic INTEGER]
                                    --socket-path SOCKET_FILE
                                    --marlowe-file MARLOWE_FILE
                                    --role-name TOKEN_NAME
                                    --tx-in-collateral TXID#TXIX [--tx-in TXID#TXIX]
                                    [--tx-out ADDRESS+VALUE]
                                    --change-address ADDRESS
                                    [--required-signer SIGNING_FILE] --out-file FILE
                                    [--submit SECONDS] [--print-stats]
                                    [--script-invalid]
      Withdraw funds from the Marlowe role address.
    
    Available options:
      --testnet-magic INTEGER         Network magic, or omit for mainnet.
      --socket-path SOCKET_FILE       Location of the cardano-node socket file.
      --marlowe-file MARLOWE_FILE     JSON file with the Marlowe inputs, final state, and final contract.
      --role-name TOKEN_NAME          The role name for the withdrawal.
      --tx-in-collateral              TXID#TXIX Collateral for transaction.
      --tx-in TXID#TXIX               Transaction input in TxId#TxIx format.
      --tx-out ADDRESS+VALUE          Transaction output in ADDRESS+VALUE format.
      --change-address ADDRESS        Address to receive ADA in excess of fee.
      --required-signer SIGNING_FILE  File containing a required signing key.
      --out-file FILE                 Output file for transaction body.
      --submit SECONDS                Also submit the transaction, and wait for confirmation.
      --print-stats                   Print statistics.
      --script-invalid                Assert that the transaction is invalid.
      -h,--help                       Show this help text
