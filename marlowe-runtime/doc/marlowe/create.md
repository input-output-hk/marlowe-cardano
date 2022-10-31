# Build a Transaction to Create a Contract

```console
Usage: marlowe create --change-address ADDRESS [-a|--address ADDRESS] 
                      [--collateral-utxo UTXO] --manual-sign FILE_PATH 
                      [-m|--metadata-file FILE_PATH] [--v1] 
                      [(-r|--role ROLE=ADDRESS) | 
                        --roles-config-file FILE_PATH | 
                        --role-token-policy-id POLICY_ID] 
                      (--core-file FILE_PATH | --contract-file FILE_PATH 
                        [--args-file FILE_PATH | 
                          [--timeout-arg NAME=POSIX_TIMESTAMP] 
                          [--value-arg NAME=INTEGER]]) --min-utxo LOVELACE

  Create a new Marlowe Contract

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
  --v1                     Run command in Marlowe V1
  -r,--role ROLE=ADDRESS   The name of a role in the contract with the address
                           to send the token to
  --roles-config-file FILE_PATH
                           A JSON file containing a map of role token names to a
                           roles configuration object. The roles configuration
                           object has two keys, "address" and "metadata", where
                           "address" is the address to send the newly minted
                           role token and "metadata" is the CIP-25 metadata
                           object to associate with the token.
  --role-token-policy-id POLICY_ID
                           The hexadecimal-encoded policy ID of the role tokens
                           for this contract. This option is used to support
                           role tokens minted in a separate transaction.
  --core-file FILE_PATH    A file containing the Core Marlowe JSON definition of
                           the contract to create.
  --contract-file FILE_PATH
                           A file containing the Extended Marlowe JSON
                           definition of the contract to create.
  --args-file FILE_PATH    A file containing the Extended Marlowe arguments to
                           apply to the contract.
  --timeout-arg NAME=POSIX_TIMESTAMP
                           The name of a timeout parameter in the contract and a
                           value to assign to it (in POSIX milliseconds).
  --value-arg NAME=INTEGER The name of a numeric parameter in the contract and a
                           value to assign to it.
  --min-utxo LOVELACE      An amount which should be used as min ADA requirement
                           for the Contract UTxO.
  -h,--help                Show this help text
```