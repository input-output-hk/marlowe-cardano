# Build a Transaction to Notify a Contract

```console
Usage: marlowe notify --change-address ADDRESS [-a|--address ADDRESS] 
                      [--collateral-utxo UTXO] --manual-sign FILE_PATH 
                      [-m|--metadata-file FILE_PATH] (-c|--contract CONTRACT_ID)
                      [--continuation-file FILE_PATH] 
                      [-l|--validity-lower-bound TIMESTAMP] 
                      [-u|--validity-upper-bound TIMESTAMP]

  Notify a contract to proceed

Available options:
  --change-address ADDRESS The address to which the change of the transaction
                           should be sent.
  -a,--address ADDRESS     An address whose UTXOs can be used as inputs to the
                           transaction
  --collateral-utxo UTXO   A UTXO which may be used as a collateral input
  --manual-sign FILE_PATH  Sign the transaction manually. Writes the CBOR bytes
                           of the unsigned transaction to the specified file for
                           manual signing. Use the submit command to submit the
                           signed transaction.
  -m,--metadata-file FILE_PATH
                           A JSON file containing a map of integer indexes to
                           arbitrary JSON values that will be added to the
                           transaction's metadata.
  -c,--contract CONTRACT_ID
                           The ID of the Marlowe contract to notify
  --continuation-file FILE_PATH
                           A file containing the continuation contract JSON for
                           making a choice in a Merkleized contract.
  -l,--validity-lower-bound TIMESTAMP
                           The lower bound of the transaction validity interval
                           in POSIX milliseconds. If not specified, the current
                           time (as determined by the Cardano node) will be
                           used.
  -u,--validity-upper-bound TIMESTAMP
                           The upper bound of the transaction validity interval
                           in POSIX milliseconds. If not specified, the next
                           timeout in the contract will be used (bounded by the
                           maximum value allowed by the Cardano node).
  -h,--help                Show this help text
```