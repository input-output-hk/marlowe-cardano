---
date: 18 June 2022
version: marlowe-cli 0.0.5.0
---

# Marlowe CLI `util` Subcommands

The `marlowe-cli util` command provide miscellanous support for Marlowe
contracts and transactions.

## Contents

-   [Decode Bech32](#decode-bech32)
-   [Encode Bech32](#encode-bech32)
-   [Slotting](#slotting)
-   [Merkleize](#merkleize)
-   [Demerkleize](#demerkleize)
-   [Clean](#clean)
-   [Faucet](#faucet)
-   [Mint](#mint)
-   [Select](#select)
-   [Watch](#watch)

## Available Commands

``` bash
marlowe-cli util --help
```

    Usage: marlowe-cli util COMMAND

      Miscellaneous utilities.

    Available options:
      -h,--help                Show this help text

    Miscellaneous low-level commands:
      clean                    Reorganize the UTxOs at an address, separating
                               tokens.
      decode-bech32            DecodBech32 data.
      demerkleize              Demerkleize a Marlowe contract.
      encode-bech32            EncodBech32 data.
      faucet                   Fund an address from a faucet. Note that the faucet
                               is only funded on the private developer testnet for
                               Marlowe, and that this command will not supply funds
                               on public networks.
      merkleize                Merkleize a Marlowe contract.
      mint                     Mint native tokens.
      select                   Select UTxO by asset.
      slotting                 Find the slot-to-time relationship for the current
                               epoch.
      watch                    Watch Marlowe transactions on a Cardano node.

## Decode Bech32

``` bash
marlowe-cli util decode-bech32 --help
```

    Usage: marlowe-cli util decode-bech32 BECH32

      DecodBech32 data.

    Available options:
      BECH32                   The Bech32 text.
      -h,--help                Show this help text

### Example

``` bash
marlowe-cli util decode-bech32 addr_test1wr2yzgn42ws0r2t9lmnavzs0wf9ndrw3hhduyzrnplxwhncaya5f8
```

    Human-readable part: addr_test
    70d441227553a0f1a965fee7d60a0f724b368dd1bddbc208730fccebcf

## Encode Bech32

``` bash
marlowe-cli util encode-bech32 --help
```

    Usage: marlowe-cli util encode-bech32 PREFIX BASE16

      EncodBech32 data.

    Available options:
      PREFIX                   The Bech32 human-readable prefix.
      BASE16                   The base 16 data to be encoded.
      -h,--help                Show this help text

### Example

``` bash
marlowe-cli util encode-bech32 addr_test 70d441227553a0f1a965fee7d60a0f724b368dd1bddbc208730fccebcf
```

    addr_test1wr2yzgn42ws0r2t9lmnavzs0wf9ndrw3hhduyzrnplxwhncaya5f8

## Slotting

``` bash
marlowe-cli util slotting --help
```

    Usage: marlowe-cli util slotting --testnet-magic INTEGER
                                     --socket-path SOCKET_FILE [--out-file FILE]

      Find the slot-to-time relationship for the current epoch.

    Available options:
      --testnet-magic INTEGER  Network magic. Defaults to the CARDANO_TESTNET_MAGIC
                               environment variable's value.
      --socket-path SOCKET_FILE
                               Location of the cardano-node socket file. Defaults to
                               the CARDANO_NODE_SOCKET_PATH environment variable's
                               value.
      --out-file FILE          Output file for slot configuration.
      -h,--help                Show this help text

### Example

This invocation of `marlowe-cli util slotting` computes the slot-to-time
parameters for the 1566 testnet.

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

## Merkleize

``` bash
marlowe-cli util merkleize --help
```

    Usage: marlowe-cli util merkleize --in-file MARLOWE_FILE 
                                      [--out-file MARLOWE_FILE]

      Merkleize a Marlowe contract.

    Available options:
      --in-file MARLOWE_FILE   The Marlowe JSON file containing the contract to be
                               merkleized.
      --out-file MARLOWE_FILE  Output file Marlowe JSON containing the merkleized
                               contract.
      -h,--help                Show this help text

### Example

Create a test contract.

``` bash
marlowe-cli run initialize --testnet-magic 1566 \
                           --socket-path node.socket \
                           --contract-file simple-1.contract \
                           --state-file simple-1.state \
                           --out-file simple-1.marlowe
```

``` bash
jq .contract simple-1.marlowe | json2yaml
```

    timeout: 1655663503000
    timeout_continuation: close
    when:
    - case:
        deposits: 12000000
        into_account:
          pk_hash: 84117ffa443e598527726f6509078114ecd1f2c031e71dd5d239d504
        of_token:
          currency_symbol: ''
          token_name: ''
        party:
          pk_hash: 84117ffa443e598527726f6509078114ecd1f2c031e71dd5d239d504
      then:
        timeout: 1655663504000
        timeout_continuation: close
        when:
        - case:
            notify_if: true
          then:
            from_account:
              pk_hash: 84117ffa443e598527726f6509078114ecd1f2c031e71dd5d239d504
            pay: 5000000
            then:
              timeout: 1655663505000
              timeout_continuation: close
              when:
              - case:
                  notify_if: true
                then: close
            to:
              party:
                pk_hash: 84117ffa443e598527726f6509078114ecd1f2c031e71dd5d239d504
            token:
              currency_symbol: ''
              token_name: ''

Now merkleize it.

``` bash
marlowe-cli util merkleize --in-file simple-1.marlowe \
                           --out-file simple-1-merkleized.marlowe
```

``` bash
jq .contract simple-1-merkleized.marlowe | json2yaml
```

    timeout: 1655663503000
    timeout_continuation: close
    when:
    - case:
        deposits: 12000000
        into_account:
          pk_hash: 84117ffa443e598527726f6509078114ecd1f2c031e71dd5d239d504
        of_token:
          currency_symbol: ''
          token_name: ''
        party:
          pk_hash: 84117ffa443e598527726f6509078114ecd1f2c031e71dd5d239d504
      merkleized_then: ded7b66715ff96e4d3cbd277fcb0e894f9f19906e6658c9fa5bce31c986002d5

Examine the continuations.

``` bash
jq .continuations simple-1-merkleized.marlowe | json2yaml
```

    - - 3878bba3343a12e37bc80007e20a5b3be885cfec00cbc494faf66e2a99b6fa97
      - from_account:
          pk_hash: 84117ffa443e598527726f6509078114ecd1f2c031e71dd5d239d504
        pay: 5000000
        then:
          timeout: 1655663505000
          timeout_continuation: close
          when:
          - case:
              notify_if: true
            then: close
        to:
          party:
            pk_hash: 84117ffa443e598527726f6509078114ecd1f2c031e71dd5d239d504
        token:
          currency_symbol: ''
          token_name: ''
    - - ded7b66715ff96e4d3cbd277fcb0e894f9f19906e6658c9fa5bce31c986002d5
      - timeout: 1655663504000
        timeout_continuation: close
        when:
        - case:
            notify_if: true
          merkleized_then: 3878bba3343a12e37bc80007e20a5b3be885cfec00cbc494faf66e2a99b6fa97

## Demerkleize

``` bash
marlowe-cli util demerkleize --help
```

    Usage: marlowe-cli util demerkleize 
             --in-file MARLOWE_FILE [--out-file MARLOWE_FILE]

      Demerkleize a Marlowe contract.

    Available options:
      --in-file MARLOWE_FILE   The Marlowe JSON file containing the contract to be
                               demerkleized.
      --out-file MARLOWE_FILE  Output file Marlowe JSON containing the demerkleized
                               contract.
      -h,--help                Show this help text

### Example

Examine an already-merkleized contract.

``` bash
jq .contract simple-1-merkleized.marlowe | json2yaml
```

    timeout: 1655663503000
    timeout_continuation: close
    when:
    - case:
        deposits: 12000000
        into_account:
          pk_hash: 84117ffa443e598527726f6509078114ecd1f2c031e71dd5d239d504
        of_token:
          currency_symbol: ''
          token_name: ''
        party:
          pk_hash: 84117ffa443e598527726f6509078114ecd1f2c031e71dd5d239d504
      merkleized_then: ded7b66715ff96e4d3cbd277fcb0e894f9f19906e6658c9fa5bce31c986002d5

Examine the continuations.

``` bash
jq .continuations simple-1-merkleized.marlowe | json2yaml
```

    - - 3878bba3343a12e37bc80007e20a5b3be885cfec00cbc494faf66e2a99b6fa97
      - from_account:
          pk_hash: 84117ffa443e598527726f6509078114ecd1f2c031e71dd5d239d504
        pay: 5000000
        then:
          timeout: 1655663505000
          timeout_continuation: close
          when:
          - case:
              notify_if: true
            then: close
        to:
          party:
            pk_hash: 84117ffa443e598527726f6509078114ecd1f2c031e71dd5d239d504
        token:
          currency_symbol: ''
          token_name: ''
    - - ded7b66715ff96e4d3cbd277fcb0e894f9f19906e6658c9fa5bce31c986002d5
      - timeout: 1655663504000
        timeout_continuation: close
        when:
        - case:
            notify_if: true
          merkleized_then: 3878bba3343a12e37bc80007e20a5b3be885cfec00cbc494faf66e2a99b6fa97

Reverse the merkliezation.

``` bash
marlowe-cli util demerkleize --in-file simple-1-merkleized.marlowe \
                             --out-file simple-1-demerkleized.marlowe
```

``` bash
jq .contract simple-1-demerkleized.marlowe | json2yaml
```

    timeout: 1655663503000
    timeout_continuation: close
    when:
    - case:
        deposits: 12000000
        into_account:
          pk_hash: 84117ffa443e598527726f6509078114ecd1f2c031e71dd5d239d504
        of_token:
          currency_symbol: ''
          token_name: ''
        party:
          pk_hash: 84117ffa443e598527726f6509078114ecd1f2c031e71dd5d239d504
      then:
        timeout: 1655663504000
        timeout_continuation: close
        when:
        - case:
            notify_if: true
          then:
            from_account:
              pk_hash: 84117ffa443e598527726f6509078114ecd1f2c031e71dd5d239d504
            pay: 5000000
            then:
              timeout: 1655663505000
              timeout_continuation: close
              when:
              - case:
                  notify_if: true
                then: close
            to:
              party:
                pk_hash: 84117ffa443e598527726f6509078114ecd1f2c031e71dd5d239d504
            token:
              currency_symbol: ''
              token_name: ''

Compare to the original contract.

``` bash
diff -s simple-1.marlowe simple-1-demerkleized.marlowe
```

    Files simple-1.marlowe and simple-1-demerkleized.marlowe are identical

## Select

``` bash
marlowe-cli util select --help
```

    Usage: marlowe-cli util select --testnet-magic INTEGER --socket-path SOCKET_FILE
                                   [--all | --lovelace-only LOVELACE | 
                                     --asset-only CURRENCY_SYMBOL.TOKEN_NAME]
                                   ADDRESS

      Select UTxO by asset.

    Available options:
      --testnet-magic INTEGER  Network magic. Defaults to the CARDANO_TESTNET_MAGIC
                               environment variable's value.
      --socket-path SOCKET_FILE
                               Location of the cardano-node socket file. Defaults to
                               the CARDANO_NODE_SOCKET_PATH environment variable's
                               value.
      --all                    Report all output.
      --lovelace-only LOVELACE The minimum Lovelace that must be the sole asset in
                               the output value.
      --asset-only CURRENCY_SYMBOL.TOKEN_NAME
                               The current symbol and token name for the sole native
                               asset in the value.
      ADDRESS                  The address.
      -h,--help                Show this help text

### Example

``` bash
$ marlowe-cli util select --testnet-magic 1566 \
                          --socket-path node.socket \
                          --lovelace-only 20000000 \
                          addr_test1vrssw4edcts00kk6lp7p5n64666m23tpprqaarmdwkaq69gfvqnpz
```

## Clean

``` bash
marlowe-cli util clean --help
```

    Usage: marlowe-cli util clean --testnet-magic INTEGER --socket-path SOCKET_FILE
                                  (--required-signer SIGNING_FILE) 
                                  [--lovelace LOVELACE] --change-address ADDRESS
                                  --out-file FILE [--submit SECONDS]

      Reorganize the UTxOs at an address, separating tokens.

    Available options:
      --testnet-magic INTEGER  Network magic. Defaults to the CARDANO_TESTNET_MAGIC
                               environment variable's value.
      --socket-path SOCKET_FILE
                               Location of the cardano-node socket file. Defaults to
                               the CARDANO_NODE_SOCKET_PATH environment variable's
                               value.
      --required-signer SIGNING_FILE
                               File containing a required signing key.
      --lovelace LOVELACE      The lovelace to send with each bundle of tokens.
      --change-address ADDRESS Address to receive ADA in excess of fee.
      --out-file FILE          Output file for transaction body.
      --submit SECONDS         Also submit the transaction, and wait for
                               confirmation.
      -h,--help                Show this help text

### Example

``` bash
$ marlowe-cli util clean --testnet-magic 1566 \
                         --socket-path node.socket \
                         --required-signer christopher-marlowe.skey \
                         --change-address addr_test1vrssw4edcts00kk6lp7p5n64666m23tpprqaarmdwkaq69gfvqnpz \
                         --out-file /dev/null \
                         --submit=600
```

## Faucet

``` bash
marlowe-cli util faucet --help
```

    Usage: marlowe-cli util faucet --testnet-magic INTEGER --socket-path SOCKET_FILE
                                   [--lovelace LOVELACE] --out-file FILE 
                                   [--submit SECONDS] ADDRESS

      Fund an address from a faucet. Note that the faucet is only funded on the
      private developer testnet for Marlowe, and that this command will not supply
      funds on public networks.

    Available options:
      --testnet-magic INTEGER  Network magic. Defaults to the CARDANO_TESTNET_MAGIC
                               environment variable's value.
      --socket-path SOCKET_FILE
                               Location of the cardano-node socket file. Defaults to
                               the CARDANO_NODE_SOCKET_PATH environment variable's
                               value.
      --lovelace LOVELACE      The lovelace to send to each address.
      --out-file FILE          Output file for transaction body.
      --submit SECONDS         Also submit the transaction, and wait for
                               confirmation.
      ADDRESS                  The addresses to receive the funds.
      -h,--help                Show this help text

### Example

``` bash
$ marlowe-cli util faucet --testnet-magic 1566 \
                          --socket-path node.socket \
                          --lovelace 100000000 \
                          --out-file /dev/null \
                          --submit 600 \
                          addr_test1vrssw4edcts00kk6lp7p5n64666m23tpprqaarmdwkaq69gfvqnpz
```

## Mint

``` bash
marlowe-cli util mint --help
```

    Usage: marlowe-cli util mint --testnet-magic INTEGER --socket-path SOCKET_FILE
                                 --required-signer SIGNING_FILE 
                                 [--metadata-file JSON_FILE] [--count INTEGER] 
                                 [--expires SLOT_NO] [--lovelace LOVELACE]
                                 --change-address ADDRESS --out-file FILE 
                                 [--submit SECONDS] TOKEN_NAME

      Mint native tokens.

    Available options:
      --testnet-magic INTEGER  Network magic. Defaults to the CARDANO_TESTNET_MAGIC
                               environment variable's value.
      --socket-path SOCKET_FILE
                               Location of the cardano-node socket file. Defaults to
                               the CARDANO_NODE_SOCKET_PATH environment variable's
                               value.
      --required-signer SIGNING_FILE
                               File containing a required signing key.
      --metadata-file JSON_FILE
                               The CIP-25 metadata, with keys for each token name.
      --count INTEGER          The number of each token to mint.
      --expires SLOT_NO        The slot number after which miniting is no longer
                               possible.
      --lovelace LOVELACE      The lovelace to send with each bundle of tokens.
      --change-address ADDRESS Address to receive ADA in excess of fee.
      --out-file FILE          Output file for transaction body.
      --submit SECONDS         Also submit the transaction, and wait for
                               confirmation.
      TOKEN_NAME               The name of the token.
      -h,--help                Show this help text

### Example

``` bash
$ marlowe-cli util mint --testnet-magic 1566 \
                        --socket-path node.socket \
                        --required-signer christopher-marlowe.skey \
                        --change-address addr_test1vrssw4edcts00kk6lp7p5n64666m23tpprqaarmdwkaq69gfvqnpz \
                        --count -1 \
                        --expires 6591100 \
                        --out-file /dev/null \
                        --submit=600 \
                        CM FB TM
```

## Watch

``` bash
marlowe-cli util watch --help
```

    Usage: marlowe-cli util watch --testnet-magic INTEGER --socket-path SOCKET_FILE 
                                  [--all] [--cbor] [--continue] 
                                  [--restart POINT_FILE] [--out-file OUTPUT_FILE]

      Watch Marlowe transactions on a Cardano node.

    Available options:
      --testnet-magic INTEGER  Network magic. Defaults to the CARDANO_TESTNET_MAGIC
                               environment variable's value.
      --socket-path SOCKET_FILE
                               Location of the cardano-node socket file. Defaults to
                               the CARDANO_NODE_SOCKET_PATH environment variable's
                               value.
      --all                    Whether to also output non-Marlowe transactions.
      --cbor                   Whether to output CBOR instead of JSON.
      --continue               Whether to continue when the current tip of the chain
                               is reached.
      --restart POINT_FILE     File for restoring and saving current point on the
                               chain.
      --out-file OUTPUT_FILE   File in which to store records of Marlowe
                               transactions.
      -h,--help                Show this help text

### Example

``` bash
$ marlowe-cli util watch --testnet-magic 1566 \
                         --socket-path node.socket
```
