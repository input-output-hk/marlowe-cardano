---
date: 17 September 2022
version: marlowe-cli 0.0.8.0
---

<div class="cell markdown">

# Marlowe CLI `util` Subcommands

The `marlowe-cli util` command provide miscellanous support for Marlowe
contracts and transactions.

</div>

<div class="cell markdown">

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

</div>

<div class="cell markdown">

## Available Commands

</div>

<div class="cell code" execution_count="1">

``` bash
marlowe-cli util --help
```

<div class="output stream stdout">

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

</div>

</div>

<div class="cell markdown">

## Decode Bech32

</div>

<div class="cell code" execution_count="2">

``` bash
marlowe-cli util decode-bech32 --help
```

<div class="output stream stdout">

    Usage: marlowe-cli util decode-bech32 BECH32

      DecodBech32 data.

    Available options:
      BECH32                   The Bech32 text.
      -h,--help                Show this help text

</div>

</div>

<div class="cell markdown">

### Example

</div>

<div class="cell code" execution_count="3">

``` bash
marlowe-cli util decode-bech32 addr_test1wr2yzgn42ws0r2t9lmnavzs0wf9ndrw3hhduyzrnplxwhncaya5f8
```

<div class="output stream stdout">

    Human-readable part: addr_test
    70d441227553a0f1a965fee7d60a0f724b368dd1bddbc208730fccebcf

</div>

</div>

<div class="cell markdown">

## Encode Bech32

</div>

<div class="cell code" execution_count="4">

``` bash
marlowe-cli util encode-bech32 --help
```

<div class="output stream stdout">

    Usage: marlowe-cli util encode-bech32 PREFIX BASE16

      EncodBech32 data.

    Available options:
      PREFIX                   The Bech32 human-readable prefix.
      BASE16                   The base 16 data to be encoded.
      -h,--help                Show this help text

</div>

</div>

<div class="cell markdown">

### Example

</div>

<div class="cell code" execution_count="5">

``` bash
marlowe-cli util encode-bech32 addr_test 70d441227553a0f1a965fee7d60a0f724b368dd1bddbc208730fccebcf
```

<div class="output stream stdout">

    addr_test1wr2yzgn42ws0r2t9lmnavzs0wf9ndrw3hhduyzrnplxwhncaya5f8

</div>

</div>

<div class="cell markdown">

## Slotting

</div>

<div class="cell code" execution_count="6">

``` bash
marlowe-cli util slotting --help
```

<div class="output stream stdout">

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

</div>

</div>

<div class="cell markdown">

### Example

This invocation of `marlowe-cli util slotting` computes the slot-to-time
parameters for the 1566 testnet.

</div>

<div class="cell code" execution_count="7">

``` bash
marlowe-cli util slotting --testnet-magic 2 \
                          --socket-path node.socket \
                          --out-file 2.slotting
```

</div>

<div class="cell code" execution_count="8">

``` bash
json2yaml 2.slotting
```

<div class="output stream stdout">

    scSlotLength: 1000
    scSlotZeroTime: 1660003200000

</div>

</div>

<div class="cell markdown">

## Merkleize

</div>

<div class="cell code" execution_count="9">

``` bash
marlowe-cli util merkleize --help
```

<div class="output stream stdout">

    Usage: marlowe-cli util merkleize --in-file MARLOWE_FILE 
                                      [--out-file MARLOWE_FILE]

      Merkleize a Marlowe contract.

    Available options:
      --in-file MARLOWE_FILE   The Marlowe JSON file containing the contract to be
                               merkleized.
      --out-file MARLOWE_FILE  Output file Marlowe JSON containing the merkleized
                               contract.
      -h,--help                Show this help text

</div>

</div>

<div class="cell markdown">

### Example

</div>

<div class="cell markdown">

Create a test contract.

</div>

<div class="cell code" execution_count="10">

``` bash
marlowe-cli run initialize --testnet-magic 2 \
                           --socket-path node.socket \
                           --contract-file simple-1.contract \
                           --state-file simple-1.state \
                           --out-file simple-1.marlowe
```

</div>

<div class="cell code" execution_count="11">

``` bash
jq .tx.contract simple-1.marlowe | json2yaml
```

<div class="output stream stdout">

    timeout: 1655663503000
    timeout_continuation: close
    when:
    - case:
        deposits: 12000000
        into_account:
          address: addr_test1vq9prvx8ufwutkwxx9cmmuuajaqmjqwujqlp9d8pvg6gupczgtm9j
        of_token:
          currency_symbol: ''
          token_name: ''
        party:
          address: addr_test1vq9prvx8ufwutkwxx9cmmuuajaqmjqwujqlp9d8pvg6gupczgtm9j
      then:
        timeout: 1655663504000
        timeout_continuation: close
        when:
        - case:
            notify_if: true
          then:
            from_account:
              address: addr_test1vq9prvx8ufwutkwxx9cmmuuajaqmjqwujqlp9d8pvg6gupczgtm9j
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
                address: addr_test1vq9prvx8ufwutkwxx9cmmuuajaqmjqwujqlp9d8pvg6gupczgtm9j
            token:
              currency_symbol: ''
              token_name: ''

</div>

</div>

<div class="cell markdown">

Now merkleize it.

</div>

<div class="cell code" execution_count="12">

``` bash
marlowe-cli util merkleize --in-file simple-1.marlowe \
                           --out-file simple-1-merkleized.marlowe
```

</div>

<div class="cell code" execution_count="13">

``` bash
jq .tx.contract simple-1-merkleized.marlowe | json2yaml
```

<div class="output stream stdout">

    timeout: 1655663503000
    timeout_continuation: close
    when:
    - case:
        deposits: 12000000
        into_account:
          address: addr_test1vq9prvx8ufwutkwxx9cmmuuajaqmjqwujqlp9d8pvg6gupczgtm9j
        of_token:
          currency_symbol: ''
          token_name: ''
        party:
          address: addr_test1vq9prvx8ufwutkwxx9cmmuuajaqmjqwujqlp9d8pvg6gupczgtm9j
      merkleized_then: 30032ba38696e2a2c9f0f37fef7fa8cc7e2063ed4511420939be0ecfa95c0587

</div>

</div>

<div class="cell markdown">

Examine the continuations.

</div>

<div class="cell code" execution_count="14">

``` bash
jq .tx.continuations simple-1-merkleized.marlowe | json2yaml
```

<div class="output stream stdout">

    - - 30032ba38696e2a2c9f0f37fef7fa8cc7e2063ed4511420939be0ecfa95c0587
      - timeout: 1655663504000
        timeout_continuation: close
        when:
        - case:
            notify_if: true
          merkleized_then: f9232556d1ec9d7bb7d767468336985f350fd3bdeb3360c10f38e620cc8b200c
    - - f9232556d1ec9d7bb7d767468336985f350fd3bdeb3360c10f38e620cc8b200c
      - from_account:
          address: addr_test1vq9prvx8ufwutkwxx9cmmuuajaqmjqwujqlp9d8pvg6gupczgtm9j
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
            address: addr_test1vq9prvx8ufwutkwxx9cmmuuajaqmjqwujqlp9d8pvg6gupczgtm9j
        token:
          currency_symbol: ''
          token_name: ''

</div>

</div>

<div class="cell markdown">

## Demerkleize

</div>

<div class="cell code" execution_count="15">

``` bash
marlowe-cli util demerkleize --help
```

<div class="output stream stdout">

    Usage: marlowe-cli util demerkleize 
             --in-file MARLOWE_FILE [--out-file MARLOWE_FILE]

      Demerkleize a Marlowe contract.

    Available options:
      --in-file MARLOWE_FILE   The Marlowe JSON file containing the contract to be
                               demerkleized.
      --out-file MARLOWE_FILE  Output file Marlowe JSON containing the demerkleized
                               contract.
      -h,--help                Show this help text

</div>

</div>

<div class="cell markdown">

### Example

</div>

<div class="cell markdown">

Examine an already-merkleized contract.

</div>

<div class="cell code" execution_count="16">

``` bash
jq .tx.contract simple-1-merkleized.marlowe | json2yaml
```

<div class="output stream stdout">

    timeout: 1655663503000
    timeout_continuation: close
    when:
    - case:
        deposits: 12000000
        into_account:
          address: addr_test1vq9prvx8ufwutkwxx9cmmuuajaqmjqwujqlp9d8pvg6gupczgtm9j
        of_token:
          currency_symbol: ''
          token_name: ''
        party:
          address: addr_test1vq9prvx8ufwutkwxx9cmmuuajaqmjqwujqlp9d8pvg6gupczgtm9j
      merkleized_then: 30032ba38696e2a2c9f0f37fef7fa8cc7e2063ed4511420939be0ecfa95c0587

</div>

</div>

<div class="cell markdown">

Examine the continuations.

</div>

<div class="cell code" execution_count="17">

``` bash
jq .tx.continuations simple-1-merkleized.marlowe | json2yaml
```

<div class="output stream stdout">

    - - 30032ba38696e2a2c9f0f37fef7fa8cc7e2063ed4511420939be0ecfa95c0587
      - timeout: 1655663504000
        timeout_continuation: close
        when:
        - case:
            notify_if: true
          merkleized_then: f9232556d1ec9d7bb7d767468336985f350fd3bdeb3360c10f38e620cc8b200c
    - - f9232556d1ec9d7bb7d767468336985f350fd3bdeb3360c10f38e620cc8b200c
      - from_account:
          address: addr_test1vq9prvx8ufwutkwxx9cmmuuajaqmjqwujqlp9d8pvg6gupczgtm9j
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
            address: addr_test1vq9prvx8ufwutkwxx9cmmuuajaqmjqwujqlp9d8pvg6gupczgtm9j
        token:
          currency_symbol: ''
          token_name: ''

</div>

</div>

<div class="cell markdown">

Reverse the merkliezation.

</div>

<div class="cell code" execution_count="18">

``` bash
marlowe-cli util demerkleize --in-file simple-1-merkleized.marlowe \
                             --out-file simple-1-demerkleized.marlowe
```

</div>

<div class="cell code" execution_count="19">

``` bash
jq .tx.contract simple-1-demerkleized.marlowe | json2yaml
```

<div class="output stream stdout">

    timeout: 1655663503000
    timeout_continuation: close
    when:
    - case:
        deposits: 12000000
        into_account:
          address: addr_test1vq9prvx8ufwutkwxx9cmmuuajaqmjqwujqlp9d8pvg6gupczgtm9j
        of_token:
          currency_symbol: ''
          token_name: ''
        party:
          address: addr_test1vq9prvx8ufwutkwxx9cmmuuajaqmjqwujqlp9d8pvg6gupczgtm9j
      then:
        timeout: 1655663504000
        timeout_continuation: close
        when:
        - case:
            notify_if: true
          then:
            from_account:
              address: addr_test1vq9prvx8ufwutkwxx9cmmuuajaqmjqwujqlp9d8pvg6gupczgtm9j
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
                address: addr_test1vq9prvx8ufwutkwxx9cmmuuajaqmjqwujqlp9d8pvg6gupczgtm9j
            token:
              currency_symbol: ''
              token_name: ''

</div>

</div>

<div class="cell markdown">

Compare to the original contract.

</div>

<div class="cell code" execution_count="20">

``` bash
diff -s simple-1.marlowe simple-1-demerkleized.marlowe
```

<div class="output stream stdout">

    Files simple-1.marlowe and simple-1-demerkleized.marlowe are identical

</div>

</div>

<div class="cell markdown">

## Select

</div>

<div class="cell code" execution_count="21">

``` bash
marlowe-cli util select --help
```

<div class="output stream stdout">

    Usage: marlowe-cli util select --testnet-magic INTEGER --socket-path SOCKET_FILE
                                   (--lovelace-only LOVELACE | 
                                     --asset-only CURRENCY_SYMBOL.TOKEN_NAME | 
                                     --all) ADDRESS

      Select UTxO by asset.

    Available options:
      --testnet-magic INTEGER  Network magic. Defaults to the CARDANO_TESTNET_MAGIC
                               environment variable's value.
      --socket-path SOCKET_FILE
                               Location of the cardano-node socket file. Defaults to
                               the CARDANO_NODE_SOCKET_PATH environment variable's
                               value.
      --lovelace-only LOVELACE The minimum Lovelace that must be the sole asset in
                               the output value.
      --asset-only CURRENCY_SYMBOL.TOKEN_NAME
                               The current symbol and token name for the sole native
                               asset in the value.
      --all                    Report all output.
      ADDRESS                  The address.
      -h,--help                Show this help text

</div>

</div>

<div class="cell markdown">

### Example

``` bash
$ marlowe-cli util select --testnet-magic 1566 \
                          --socket-path node.socket \
                          --lovelace-only 20000000 \
                          addr_test1vrssw4edcts00kk6lp7p5n64666m23tpprqaarmdwkaq69gfvqnpz
```

</div>

<div class="cell markdown">

## Clean

</div>

<div class="cell code" execution_count="22">

``` bash
marlowe-cli util clean --help
```

<div class="output stream stdout">

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

</div>

</div>

<div class="cell markdown">

### Example

``` bash
$ marlowe-cli util clean --testnet-magic 1566 \
                         --socket-path node.socket \
                         --required-signer christopher-marlowe.skey \
                         --change-address addr_test1vrssw4edcts00kk6lp7p5n64666m23tpprqaarmdwkaq69gfvqnpz \
                         --out-file /dev/null \
                         --submit=600
```

</div>

<div class="cell markdown">

## Faucet

</div>

<div class="cell code" execution_count="23">

``` bash
marlowe-cli util faucet --help
```

<div class="output stream stdout">

    Usage: marlowe-cli util faucet --testnet-magic INTEGER --socket-path SOCKET_FILE
                                   [--lovelace LOVELACE] --out-file FILE 
                                   [--submit SECONDS] --faucet-address ADDRESS
                                   --required-signer SIGNING_FILE ADDRESS

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
      --faucet-address ADDRESS The faucet addresses to provide funds.
      --required-signer SIGNING_FILE
                               File containing a required signing key.
      ADDRESS                  The addresses to receive the funds.
      -h,--help                Show this help text

</div>

</div>

<div class="cell markdown">

### Example

``` bash
$ marlowe-cli util faucet --testnet-magic 1566 \
                          --socket-path node.socket \
                          --lovelace 100000000 \
                          --faucet-address addr_test1vrssw4edcts00kk6lp7p5n64666m23tpprqaarmdwkaq69gfvqnpz \
                          --required-signer faucet.skey \
                          --out-file /dev/null \
                          --submit 600 \
                          addr_test1vrssw4edcts00kk6lp7p5n64666m23tpprqaarmdwkaq69gfvqnpz
```

</div>

<div class="cell markdown">

## Mint

</div>

<div class="cell code" execution_count="24">

``` bash
marlowe-cli util mint --help
```

<div class="output stream stdout">

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

</div>

</div>

<div class="cell markdown">

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

</div>

<div class="cell markdown">

## Watch

</div>

<div class="cell code" execution_count="25">

``` bash
marlowe-cli util watch --help
```

<div class="output stream stdout">

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

</div>

</div>

<div class="cell markdown">

### Example

``` bash
$ marlowe-cli util watch --testnet-magic 1566 \
                         --socket-path node.socket
```

</div>
