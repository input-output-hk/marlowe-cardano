# Marlowe CLI: Run a Marlowe Transaction

The `marlowe-cli run execute` command creates and optionally runs a Marlowe transaction.


## Usage

    $ marlowe-cli run execute --help
    
    Usage: marlowe-cli run execute [--testnet-magic INTEGER]
                                   --socket-path SOCKET_FILE
                                   [--marlowe-in-file MARLOWE_FILE
                                     --tx-in-marlowe TXID#TXIX
                                     --tx-in-collateral TXID#TXIX]
                                   --marlowe-out-file MARLOWE_FILE
                                   [--tx-in TXID#TXIX] [--tx-out ADDRESS+VALUE]
                                   --change-address ADDRESS
                                   [--required-signer SIGNING_FILE] --out-file FILE
                                   [--submit SECONDS] [--print-stats]
                                   [--script-invalid]
      Run a Marlowe transaction.
    
    Available options:
      --testnet-magic INTEGER          Network magic, or omit for mainnet.
      --socket-path SOCKET_FILE        Location of the cardano-node socket file.
      --marlowe-in-file MARLOWE_FILE   JSON file with the Marlowe initial state and initial contract, if any.
      --tx-in-marlowe TXID#TXIX        UTxO spent from Marlowe contract, if any.
      --tx-in-collateral TXID#TXIX     Collateral for transaction, if any.
      --marlowe-out-file MARLOWE_FILE  JSON file with the Marlowe inputs, final state, and final contract.
      --tx-in TXID#TXIX                Transaction input in TxId#TxIx format.
      --tx-out ADDRESS+VALUE           Transaction output in ADDRESS+VALUE format.
      --change-address ADDRESS         Address to receive ADA in excess of fee.
      --required-signer SIGNING_FILE   File containing a required signing key.
      --out-file FILE                  Output file for transaction body.
      --submit SECONDS                 Also submit the transaction, and wait for confirmation.
      --print-stats                    Print statistics.
      --script-invalid                 Assert that the transaction is invalid.
      -h,--help                        Show this help text
