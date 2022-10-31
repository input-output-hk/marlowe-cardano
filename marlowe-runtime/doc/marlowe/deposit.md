# Build a Transaction to Deposit Funds into a Contract

```console
Usage: marlowe deposit --change-address ADDRESS [-a|--address ADDRESS] 
                       [--collateral-utxo UTXO] --manual-sign FILE_PATH 
                       [-m|--metadata-file FILE_PATH]
                       (-c|--contract CONTRACT_ID) --to-party ROLE_NAME|ADDRESS
                       --from-party ROLE_NAME|ADDRESS 
                       ((-c|--currency MINTING_POLICY_ID)
                         (-n|--token-name TOKEN_NAME) (-q|--quantity INTEGER) |
                         (-l|--lovelace INTEGER)) 
                       [--continuation-file FILE_PATH] 
                       [-l|--validity-lower-bound TIMESTAMP] 
                       [-u|--validity-upper-bound TIMESTAMP]

  Deposit funds into a contract

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
                           The ID of the Marlowe contract to deposit funds into
  --to-party ROLE_NAME|ADDRESS
                           The party into whose account to deposit the funds.
  --from-party ROLE_NAME|ADDRESS
                           The party depositing the funds.
  -c,--currency MINTING_POLICY_ID
                           The minting policy ID of the token(s) to deposit.
  -n,--token-name TOKEN_NAME
                           The name of the token(s) to deposit.
  -q,--quantity INTEGER    The quantity of tokens to deposit.
  -l,--lovelace INTEGER    The quantity of lovelace to deposit.
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