---
date: 18 June 2022
version: marlowe-cli 0.0.5.0
---

# Marlowe CLI `query` Subcommands

The `marlowe-cli query` commands perform chain-index queries to discover
the Marlowe-related history on the Cardano blockchain.

## Contents

-   [Address](#address)
-   [App](#app)
-   [History](#history)
-   [Output](#output)
-   [Payout](#payout)
-   [Transaction](#transaction)

## Available Commands

``` bash
marlowe-cli query --help
```

    Usage: marlowe-cli query COMMAND

      Blockchain queries for Marlowe.

    Available options:
      -h,--help                Show this help text

    Query commands:
      address                  Query transactions at an address.
      app                      Query the state of the Marlowe application script.
      history                  Query for the Marlowe contract histories.
      output                   Query output details.
      payout                   Query the state of the Marlowe payout script.
      transaction              Query transaction details.

## Address

``` bash
marlowe-cli query address --help
```

    Usage: marlowe-cli query address --index-url URL [--spent] 
                                     [--out-file OUTPUT_FILE] ADDRESS

      Query transactions at an address.

    Available options:
      --index-url URL          URL for the Plutus chain index.
      --spent                  Whether to also report spent transactions.
      --out-file OUTPUT_FILE   JSON output file for address data.
      ADDRESS                  The address.
      -h,--help                Show this help text

### Example

``` bash
marlowe-cli query address --index-url http://localhost:9083 \
                          addr_test1vzzpzll6gsl9npf8wfhk2zg8sy2we50jcqc7w8w46gua2pqq7cw2q \
| json2yaml \
| head -n 20
```

    - _citxCardanoTx:
        eraInMode: AlonzoEraInCardanoMode
        tx:
          cborHex: 84a5008482582073f53d385eb75f890430c9b06fb74f2dbb1395fc6c7051774977fe63464c02b50182582073f53d385eb75f890430c9b06fb74f2dbb1395fc6c7051774977fe63464c02b502825820f7769579b2cb976afecd8e1b20d2b22d78050d44f4e63e5a038cd89e7e7a65ab01825820ff68d04b699ecba76f1e3d7bdf803573828639cfa5863171788c5f000b72b9c3020d80018482581d6084117ffa443e598527726f6509078114ecd1f2c031e71dd5d239d5041a02f7fc5b82581d6084117ffa443e598527726f6509078114ecd1f2c031e71dd5d239d504821a001e8480a1581c8af8951db9f0ee17a17f5c91d7579854197316f21185a5d83438a0dba14246420182581d6084117ffa443e598527726f6509078114ecd1f2c031e71dd5d239d504821a001e8480a1581ca72e9977bf3079d0c5143a8e859f6b3a6ca368083f3680fb6bc202b5a14246420182581d6084117ffa443e598527726f6509078114ecd1f2c031e71dd5d239d504821a001e8480a1581cd0e2ebf0a20c10d870d447854d178b2b0928ae1ce8661a01acfc662fa142464201021a0002f4250e80a100818258204b6a0fb7d62fbc0e5894ec95d66ca73f110f224fe1d383acaf1c051994472473584070ce933ec5d104c550ab47e80273d1d233823254f0f28468a36ee07789f8ccf0ac4470482c12d57d7ca5d4f20e6e751817eafca3df8afc3fdb99b13f54aafe08f5f6
          description: ''
          type: Tx AlonzoEra
      _citxData: []
      _citxInputs:
      - txInRef:
          txOutRefId:
            getTxId: 73f53d385eb75f890430c9b06fb74f2dbb1395fc6c7051774977fe63464c02b5
          txOutRefIdx: 1
        txInType: null
      - txInRef:
          txOutRefId:
            getTxId: 73f53d385eb75f890430c9b06fb74f2dbb1395fc6c7051774977fe63464c02b5
          txOutRefIdx: 2
        txInType: null
      - txInRef:
          txOutRefId:

## App

``` bash
marlowe-cli query app --help
```

    Usage: marlowe-cli query app --index-url URL [--roles-currency CURRENCY_SYMBOL] 
                                 [--spent] [--out-file OUTPUT_FILE]

      Query the state of the Marlowe application script.

    Available options:
      --index-url URL          URL for the Plutus chain index.
      --roles-currency CURRENCY_SYMBOL
                               The currency symbol for roles, if any.
      --spent                  Whether to also report spent transactions.
      --out-file OUTPUT_FILE   JSON output file for Marlowe data.
      -h,--help                Show this help text

### Example

``` bash
marlowe-cli query app --index-url http://localhost:9083 \
                      --roles-currency ce78aa73ad62a31e98a76b7952a25fa90fa510e1e2ef473d001274ff \
                      --spent \
| json2yaml \
| head -n 20
```

    - marloweDatum:
        marloweContract:
          timeout: 1929587625000
          timeout_continuation: close
          when:
          - case:
              deposits: 15000000
              into_account:
                role_token: PAB
              of_token:
                currency_symbol: ''
                token_name: ''
              party:
                role_token: PAB
            then:
              timeout: 1961123625000
              timeout_continuation: close
              when:
              - case:
                  notify_if: true

## History

``` bash
marlowe-cli query history --help
```

    Usage: marlowe-cli query history --slot-length INTEGER --slot-offset INTEGER
                                     --index-url URL 
                                     [--roles-currency CURRENCY_SYMBOL] 
                                     [--out-file OUTPUT_FILE]

      Query for the Marlowe contract histories.

    Available options:
      --slot-length INTEGER    The slot length, in milliseconds.
      --slot-offset INTEGER    The effective POSIX time of slot zero, in
                               milliseconds.
      --index-url URL          URL for the Plutus chain index.
      --roles-currency CURRENCY_SYMBOL
                               The currency symbol for roles, if any.
      --out-file OUTPUT_FILE   JSON output file for history data.
      -h,--help                Show this help text

### Example

``` bash
marlowe-cli util slotting --testnet-magic 1566 \
                          --socket-path node.socket \
                          --out-file 1566.slotting
```

``` bash
json2yaml 1566.slotting
```

    scSlotLength: 1000
    scSlotZeroTime: 1649976791000

``` bash
marlowe-cli query history --slot-length $(jq .scSlotLength 1566.slotting) \
                          --slot-offset $(jq .scSlotZeroTime 1566.slotting) \
                          --index-url http://localhost:9083 \
                          --roles-currency ce78aa73ad62a31e98a76b7952a25fa90fa510e1e2ef473d001274ff \
| json2yaml \
| head -n 20
```

    - historyData:
        marloweContract:
          timeout: 1929587625000
          timeout_continuation: close
          when:
          - case:
              deposits: 15000000
              into_account:
                role_token: PAB
              of_token:
                currency_symbol: ''
                token_name: ''
              party:
                role_token: PAB
            then:
              timeout: 1961123625000
              timeout_continuation: close
              when:
              - case:
                  notify_if: true

## Output

``` bash
marlowe-cli query output --help
```

    Usage: marlowe-cli query output --index-url URL 
                                    [--all | --lovelace-only LOVELACE | 
                                      --asset-only CURRENCY_SYMBOL.TOKEN_NAME] 
                                    [--spent] [--out-file OUTPUT_FILE] ADDRESS

      Query output details.

    Available options:
      --index-url URL          URL for the Plutus chain index.
      --all                    Report all output.
      --lovelace-only LOVELACE The minimum Lovelace that must be the sole asset in
                               the output value.
      --asset-only CURRENCY_SYMBOL.TOKEN_NAME
                               The current symbol and token name for the sole native
                               asset in the value.
      --spent                  Whether to also report spent transactions.
      --out-file OUTPUT_FILE   JSON output file for address data.
      ADDRESS                  The address.
      -h,--help                Show this help text

### Example

``` bash
marlowe-cli query output --index-url http://localhost:9083 \
                         addr_test1vzzpzll6gsl9npf8wfhk2zg8sy2we50jcqc7w8w46gua2pqq7cw2q \
| json2yaml \
| head -n 20
```

    - - txOutRefId:
          getTxId: e338b26be569e3dc79f9a07c4d75ab79fdde9534860f984635556dac3a620b1c
        txOutRefIdx: 1
      - _ciTxOutAddress:
          addressCredential:
            contents:
              getPubKeyHash: 84117ffa443e598527726f6509078114ecd1f2c031e71dd5d239d504
            tag: PubKeyCredential
          addressStakingCredential: null
        _ciTxOutValue:
          getValue:
          - - unCurrencySymbol: ''
            - - - unTokenName: ''
                - 2000000
          - - unCurrencySymbol: 8af8951db9f0ee17a17f5c91d7579854197316f21185a5d83438a0db
            - - - unTokenName: FB
                - 1
        tag: PublicKeyChainIndexTxOut
    - - txOutRefId:
          getTxId: e338b26be569e3dc79f9a07c4d75ab79fdde9534860f984635556dac3a620b1c

## Payout

``` bash
marlowe-cli query payout --help
```

    Usage: marlowe-cli query payout --index-url URL 
                                    [--roles-currency CURRENCY_SYMBOL] [--spent] 
                                    [--out-file OUTPUT_FILE]

      Query the state of the Marlowe payout script.

    Available options:
      --index-url URL          URL for the Plutus chain index.
      --roles-currency CURRENCY_SYMBOL
                               The currency symbol for roles, if any.
      --spent                  Whether to also report spent transactions.
      --out-file OUTPUT_FILE   JSON output file for payout data.
      -h,--help                Show this help text

### Example

``` bash
marlowe-cli query payout --index-url http://localhost:9083 \
                         --roles-currency ce78aa73ad62a31e98a76b7952a25fa90fa510e1e2ef473d001274ff \
                         --spent \
| json2yaml \
| head -n 20
```

    - marloweDatum:
        marloweContract:
          timeout: 1929587625000
          timeout_continuation: close
          when:
          - case:
              deposits: 15000000
              into_account:
                role_token: PAB
              of_token:
                currency_symbol: ''
                token_name: ''
              party:
                role_token: PAB
            then:
              timeout: 1961123625000
              timeout_continuation: close
              when:
              - case:
                  notify_if: true

## Transaction

``` bash
marlowe-cli query transaction --help
```

    Usage: marlowe-cli query transaction 
             --index-url URL [--out-file OUTPUT_FILE] TXID

      Query transaction details.

    Available options:
      --index-url URL          URL for the Plutus chain index.
      --out-file OUTPUT_FILE   JSON output file for transaction data.
      TXID                     The transaction ID.
      -h,--help                Show this help text

### Example

``` bash
marlowe-cli query transaction --index-url http://localhost:9083 \
                              8d85c3a104d0c9148aa14bbf0f4b042cff9f7a62bcc02644f3c5ffb10ebad6ce \
| json2yaml \
| head -n 20
```

    - _citxCardanoTx:
        eraInMode: AlonzoEraInCardanoMode
        tx:
          cborHex: 84a5008582582025356d58106f8f7e401ab3aa0b2a960c54f577797af0b04115c4fe38502e50e90182582025356d58106f8f7e401ab3aa0b2a960c54f577797af0b04115c4fe38502e50e90282582063a6abf52df55128e63baa72f52270896cb5d73154edcb317a352d49a4559771018258207b68db3624e8ad82c8825996bfd6910716409226617ee5fcfdc01e77d59c2b41028258208019ea71e0fdf66c3cfbd4d7496980b3b6650a7a809b9a21557f82d31d849b20010d80018482581d6084117ffa443e598527726f6509078114ecd1f2c031e71dd5d239d5041a123a24cf82581d6084117ffa443e598527726f6509078114ecd1f2c031e71dd5d239d504821a001e8480a1581c8af8951db9f0ee17a17f5c91d7579854197316f21185a5d83438a0dba14246420182581d6084117ffa443e598527726f6509078114ecd1f2c031e71dd5d239d504821a001e8480a1581ca72e9977bf3079d0c5143a8e859f6b3a6ca368083f3680fb6bc202b5a14246420182581d6084117ffa443e598527726f6509078114ecd1f2c031e71dd5d239d504821a001e8480a1581cd494046cae145c146af0a581a42f596097179720d6cf7ce38b5ddf6fa142464201021a00030bb10e80a100818258204b6a0fb7d62fbc0e5894ec95d66ca73f110f224fe1d383acaf1c05199447247358404797b4f444d80a0947c5e45118f1a6aa11f2ec87dec127afced3f2759d3eb8113e1d34a6bd7bb1e1bfd9fc12ac638497e1aa91314a59b2ed85f8b9cad472d803f5f6
          description: ''
          type: Tx AlonzoEra
      _citxData: []
      _citxInputs:
      - txInRef:
          txOutRefId:
            getTxId: 25356d58106f8f7e401ab3aa0b2a960c54f577797af0b04115c4fe38502e50e9
          txOutRefIdx: 1
        txInType: null
      - txInRef:
          txOutRefId:
            getTxId: 25356d58106f8f7e401ab3aa0b2a960c54f577797af0b04115c4fe38502e50e9
          txOutRefIdx: 2
        txInType: null
      - txInRef:
          txOutRefId:
