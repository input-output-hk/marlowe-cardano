# Build a Transaction to Withdraw Funds Paid by a Contract

```console
Usage: marlowe withdraw --change-address ADDRESS [-a|--address ADDRESS] 
                        [--collateral-utxo UTXO] --manual-sign FILE_PATH 
                        [-m|--metadata-file FILE_PATH]
                        (-c|--contract CONTRACT_ID) [--v1] --role ROLE_NAME

  Withdraw funds paid to a role in a contract

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
                           The ID of the Marlowe contract from which to withdraw
                           funds.
  --v1                     Run command in Marlowe V1
  --role ROLE_NAME         The name of the role from which to withdraw funds.
  -h,--help                Show this help text
```