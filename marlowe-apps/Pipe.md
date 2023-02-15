# A Pipe Client for Marlowe Runtime

This [Marlowe Runtime](../marlowe-runtime) client reads JSON requests from standard input and writes JSON responses to standard output. The service may be used with any Marlowe contract.


## Example

```bash
marlowe-pipe << EOI 2> /dev/null
{"request" : "get", "contractId" : "f06e4b760f2d9578c8088ea5289ba6276e68ae3cbaf5ad27bcfc77dc413890db#1"}
EOI
```
```console
{"creation":{"output":{"address":"706a9391d6aa51af28dd876ebb5565b69d1e83e5ac7861506bd29b56b0","assets":{"ada":3000000,"tokens":[]},"datum":{"marloweContract":{"timeout":1667314499000,"timeout_continuation":"close","when":[{"case":{"deposits":{"negate":{"negate":100000000}},"into_account":{"address":"addr_test1vpwu7nklk38f3tp4yr759lssakgwhtmyprhaexlvd3aj9xs7ud88k"},"of_token":{"currency_symbol":"","token_name":""},"party":{"address":"addr_test1vpwu7nklk38f3tp4yr759lssakgwhtmyprhaexlvd3aj9xs7ud88k"}},"then":{"from_account":{"address":"addr_test1vpwu7nklk38f3tp4yr759lssakgwhtmyprhaexlvd3aj9xs7ud88k"},"pay":100000000,"then":{"timeout":1667314799000,"timeout_continuation":"close","when":[{"case":{"deposits":2000000,"into_account":{"address":"addr_test1vrzx2ys48s6k03f5hq0mc4vuwuuxq2sj3p8gsnk47z27qdqzjjmup"},"of_token":{"currency_symbol":"","token_name":""},"party":{"address":"addr_test1vrzx2ys48s6k03f5hq0mc4vuwuuxq2sj3p8gsnk47z27qdqzjjmup"}},"then":{"from_account":{"address":"addr_test1vrzx2ys48s6k03f5hq0mc4vuwuuxq2sj3p8gsnk47z27qdqzjjmup"},"pay":2000000,"then":{"timeout":1667314799000,"timeout_continuation":"close","when":[{"case":{"deposits":100000000,"into_account":{"address":"addr_test1vrzx2ys48s6k03f5hq0mc4vuwuuxq2sj3p8gsnk47z27qdqzjjmup"},"of_token":{"currency_symbol":"","token_name":""},"party":{"address":"addr_test1vrzx2ys48s6k03f5hq0mc4vuwuuxq2sj3p8gsnk47z27qdqzjjmup"}},"then":{"from_account":{"address":"addr_test1vrzx2ys48s6k03f5hq0mc4vuwuuxq2sj3p8gsnk47z27qdqzjjmup"},"pay":100000000,"then":"close","to":{"party":{"address":"addr_test1vpwu7nklk38f3tp4yr759lssakgwhtmyprhaexlvd3aj9xs7ud88k"}},"token":{"currency_symbol":"","token_name":""}}}]},"to":{"party":{"address":"addr_test1vpwu7nklk38f3tp4yr759lssakgwhtmyprhaexlvd3aj9xs7ud88k"}},"token":{"currency_symbol":"","token_name":""}}}]},"to":{"party":{"address":"addr_test1vrzx2ys48s6k03f5hq0mc4vuwuuxq2sj3p8gsnk47z27qdqzjjmup"}},"token":{"currency_symbol":"","token_name":""}}}]},"marloweParams":{"rolesCurrency":""},"marloweState":{"accounts":[[[{"address":"addr_test1vpwu7nklk38f3tp4yr759lssakgwhtmyprhaexlvd3aj9xs7ud88k"},{"currency_symbol":"","token_name":""}],3000000]],"boundValues":[],"choices":[],"minTime":0}},"utxo":{"txId":"f06e4b760f2d9578c8088ea5289ba6276e68ae3cbaf5ad27bcfc77dc413890db","txIx":1}},"payoutValidatorHash":"49076eab20243dc9462511fb98a9cfb719f86e9692288139b7c91df3"},"response":"info","steps":[{"contractId":"f06e4b760f2d9578c8088ea5289ba6276e68ae3cbaf5ad27bcfc77dc413890db#1","payouts":[],"redeemer":[],"scriptOutput":null,"step":"apply","txId":"5a3ed57653b4635c76d2949558f3718e34324a8a1ffc740360ae7d85839de6d9"}]}
```


## API

The JSON API for Marlowe Lambda contains the following requests and responses.

| Request                             | Response                  |
|-------------------------------------|---------------------------|
| [`list`](#list-contracts-ids)       | [`contracts`](#contracts) |
| [`headers`](#list-contract-headers) | [`headers`](#headers)     |
| [`get`](#get)                       | [`info`](#info)           |
| [`create`](#create)                 | [`body`](#body)           |
| [`apply`](#apply)                   | [`body`](#body)           |
| [`withdraw`](#withdraw)             | [`body`](#body)           |
| [`sign`](#sign)                     | [`tx`](#tx)               |
| [`submit`](#submit)                 | [`txId`](#txid)           |
| [`wait`](#wait)                     | [`txInfo`](#txinfo)       |


### Requests


#### List Contract IDs

List all Marlowe contract IDs on the blockchain.

```json
{
  "request" : "list"
}
```

The response is [Contracts](#contracts).


#### List Contract Headers

List all Marlowe contract headers on the blockchain.

```json
{
  "request" : "headers"
}
```

The response is [Headers](#headers).


#### Get

Fetch the history of a Marlowe contract.

```json
{
  "request" : "get"
, "contractId" : "66e4af5ec99f09c28c6378e1d9d9508d95544e258fd8c8e7f3ff168a73e7b656#1"
}
```

The response is [Info](#info).


#### Create

Build a transaction that creates a new Marlowe contract.

```json
{
  "request" : "create"
, "contract" : "/* the serialized Marlowe contract to be created */"
, "minUtxo" : "/* the number of lovelace to send to store in the contract when it is created */"
, "roles" : "/* an object that maps role names to the addresses to which the corresponding role tokens should be sent after it is minted */"
, "metadata" : "/* the transaction metadata in JSON format */"
, "addresses" : "/* the list of addresses, in addition to the change address, where UTxOs can be used as input to the transaction */"
, "change" : "/* the address to which change from the transaction will be sent */"
, "collateral" : "/* the list of UTxOs that may be used for collateral, or an empty list if any UTxO may be used for collateral */"
}
```

See `Contract` in https://github.com/input-output-hk/marlowe/blob/master/isabelle/haskell/MarloweCoreJson.lhs for definition of the JSON format of a Marlowe contract.

The response is [Body](#body).


#### Apply

Build a transaction that applies input to a Marlowe contract.

```json
{
  "request" : "apply"
, "contractId" : "/* the contract ID to which inputs will be applied */"
, "inputs" : "/* the inputs that will be applied to the contract */"
, "validityLowerBound" : "/* the POSIX time in integer milliseconds before which the transaction is not valid */"
, "validityUpperBound" : "/* the POSIX time in integer milliseconds after which the transaction is not valid */"
, "metadata" : "/* the transaction metadata in JSON format */"
, "addresses" : "/* the list of addresses, in addition to the change address, where UTxOs can be used as input to the transaction */"
, "change" : "/* the address to which change from the transaction will be sent */"
, "collateral" : "/* the list of UTxOs that may be used for collateral, or an empty list if any UTxO may be used for collateral */"
}
```

See `Input` in https://github.com/input-output-hk/marlowe/blob/master/isabelle/haskell/MarloweCoreJson.lhs for definition of the JSON format of inputs to a Marlowe contract.

The response is [Body](#body).


#### Withdraw

Build a transaction that withdraws funds paid by a Marlowe contract.

```json
{
  "request" : "withdraw"
, "contractId" : "/* the contract ID from which funds will be withdrawn */"
, "role" : "/* the name of the role making the withdrawal */"
, "addresses" : "/* the list of addresses, in addition to the change address, where UTxOs can be used as input to the transaction */"
, "change" : "/* the address to which change from the transaction will be sent */"
, "collateral" : "/* the list of UTxOs that may be used for collateral, or an empty list if any UTxO may be used for collateral */"
}
```

The response is [Body](#body).


#### Sign

Sign a transaction body with payment keys.

DO NOT USE THIS FUNCTION TO SIGN TRANSACTIONS BECAUSE IT TRANSMITS PRIVATE (SIGNING) KEYS.

```json
{
  "request" : "sign"
, "body" : "/* the transaction body in Cardano text-envelope format */"
, "paymentKeys" : "/* list of payment keys, in Cardano text-envelope format, that should sign the transaction */"
, "paymentExtendedKeys" : "/* a list of payment extended keys, in Cardano text-envelope format, that should sign the transaction */"
}
```

The response is [Tx](#tx).


#### Submit

Submit a signed transaction to the Cardano node.

```json
{
  "request" : "submit"
, "tx" : "/* the transaction in Cardano text-envelope format */"
}
```

The response is [TxId](#txid).


#### Wait

Wait for the first confirmation of a transaction on the Cardano node.

```json
{
  "request" : "wait"
, "txId" : "/* the identifier of the transaction */"
, "pollingSeconds" : "/* the number of seconds to wait between pollings of the Cardano node for confirmation */"
}
```

The response is [TxInfo](#txinfo).


### Responses


#### Contracts

This response is a list of contract IDs.

```json
{
  "response" : "contracts"
, "contractIds": [
    "f06e4b760f2d9578c8088ea5289ba6276e68ae3cbaf5ad27bcfc77dc413890db#1"
  , ...
  ]
}
```


#### Headers

This response is a list of contract headers.

```json
{
  "response" : "headers"
, "contractHeaders": [
    {
      "blockHeader" : {
        "blockNo" : 314179
      , "headerHash" : "933b51fba96bdec461e3e0dab67f3134e5b55b2a5aa38dcf94ffea8740309542"
      , "slotNo" : 13555170
      }
    , "contractId" : "93f9b8064d50bddb29b241b41e2d4e9ec49c85e05ede3f695e23d2507d5dbf37#1"
    , "marloweScriptAddress" : "706a9391d6aa51af28dd876ebb5565b69d1e83e5ac7861506bd29b56b0"
    , "marloweScriptHash" : "6a9391d6aa51af28dd876ebb5565b69d1e83e5ac7861506bd29b56b0"
    , "marloweVersion" : "v1"
    , "metadata" : {}
    , "payoutScriptHash" : "49076eab20243dc9462511fb98a9cfb719f86e9692288139b7c91df3"
    , "rolesCurrency" : ""
    }
  , ...
  ]
}
```


#### Result

This response is a simple `true`/`false` indication.

```json
{
  "response" : "result"
, "result" : true
}
```


#### Info

This response details the history of a Marlowe contract.

```json
{
  "response" : "info"
, "creation" : "/* information about the transaction that created the contract */"
, "steps" : [
    "/* information about each subsequent transaction involving the contract */"
  , ...
  ]
}
```

See `CreateStep` and `ContractStep` in [`Language.Marlowe.Runtime.History.Api`](../marlowe-runtime/history-api/Language/Marlowe/Runtime/History/Api.hs) for details on the contents of the `creation` and `steps` fields.


#### Body

This response contains a serialized transaction body.

```json
{
  "response" : "body"
, "txId" : "66e4af5ec99f09c28c6378e1d9d9508d95544e258fd8c8e7f3ff168a73e7b656"
, "contractId" : "66e4af5ec99f09c28c6378e1d9d9508d95544e258fd8c8e7f3ff168a73e7b656#1"
, "body" : "/* the JSON-serialized transaction body in Cardano text-envelope format */"
}
```


#### Tx

This response contains a serialized transaction.

```json
{
  "response" : "tx"
, "txId" : "66e4af5ec99f09c28c6378e1d9d9508d95544e258fd8c8e7f3ff168a73e7b656"
, "tx" : "/* the JSON-serialized transaction in Cardano text-envelope format */"
}
```


#### TxId

This response contains a transaction ID.

```json
{
  "response" : "txId"
, "txId" : "66e4af5ec99f09c28c6378e1d9d9508d95544e258fd8c8e7f3ff168a73e7b656"
}
```


#### TxInfo

This response contains full information about a transaction.

```json
{
  "response" : "txInfo"
, "transaction" : "/* information about the transaction */"
}
```

See `Transaction` in [`Language.Marlowe.Runtime.Core.Api`](../marlowe-runtime/src/Language/Marlowe/Runtime/Core/Api.hs) for details on the contents of the `transaction` field.


## Help


### Marlowe Pipe

```console
$ marlowe-pipe --help

marlowe-pipe: run marlowe application requests

Usage: marlowe-pipe [--chain-sync-host HOST_NAME]
                    [--chain-sync-command-port PORT_NUMBER]
                    [--chain-sync-port PORT_NUMBER]
                    [--marlowe-sync-host HOST_NAME]
                    [--marlowe-sync-port PORT_NUMBER]
                    [--marlowe-header-port PORT_NUMBER]
                    [--marlowe-query-port PORT_NUMBER] [--tx-host HOST_NAME]
                    [--tx-command-port PORT_NUMBER] [--timeout-seconds INTEGER]
                    [--build-seconds INTEGER] [--confirm-seconds INTEGER]
                    [--retry-seconds INTEGER] [--retry-limit INTEGER]

  This command-line tool reads lines of JSON from standard input, interpets them
  as Marlowe App requests, executes them, and prints the response JSON on
  standard output.

Available options:
  -h,--help                Show this help text
  --chain-sync-host HOST_NAME
                           The hostname of the Marlowe Runtime chain-sync
                           server. Can be set as the environment variable
                           MARLOWE_CHAIN_SYNC_HOST (default: "127.0.0.1")
  --chain-sync-command-port PORT_NUMBER
                           The port number of the chain-sync server's job API.
                           Can be set as the environment variable
                           MARLOWE_CHAIN_SYNC_COMMAND_PORT (default: 3720)
  --chain-sync-port PORT_NUMBER
                           The port number of the chain-sync server's
                           synchronization API. Can be set as the environment
                           variable MARLOWE_CHAIN_SYNC_PORT (default: 3715)
  --marlowe-sync-host HOST_NAME
                           The hostname of the Marlowe Runtime marlowe-sync
                           server. Can be set as the environment variable
                           MARLOWE_RT_SYNC_HOST (default: "127.0.0.1")
  --marlowe-sync-port PORT_NUMBER
                           The port number of the marlowe-sync server's
                           synchronization API. Can be set as the environment
                           variable MARLOWE_RT_SYNC_MARLOWE_SYNC_PORT
                           (default: 3724)
  --marlowe-header-port PORT_NUMBER
                           The port number of the marlowe-sync server's header
                           synchronization API. Can be set as the environment
                           variable MARLOWE_RT_SYNC_MARLOWE_HEADER_PORT
                           (default: 3725)
  --marlowe-query-port PORT_NUMBER
                           The port number of the marlowe-sync server's query
                           API. Can be set as the environment variable
                           MARLOWE_RT_SYNC_MARLOWE_QUERY_PORT (default: 3726)
  --tx-host HOST_NAME      The hostname of the Marlowe Runtime transaction
                           server. Can be set as the environment variable
                           MARLOWE_RT_TX_HOST (default: "127.0.0.1")
  --tx-command-port PORT_NUMBER
                           The port number of the transaction server's job API.
                           Can be set as the environment variable
                           MARLOWE_RT_TX_COMMAND_PORT (default: 3723)
  --timeout-seconds INTEGER
                           Timeout in seconds for transaction confirmation.
                           (default: 600)
  --build-seconds INTEGER  Wait specified seconds before transaction
                           construction. No waiting occurs if a non-positive
                           number of seconds is specified. The specified wait
                           period is randomly increased up to a factor of two.
                           Increasing this value will increase the probability
                           that Marlowe Runtime's node has seen the transactions
                           that the submitting node has seen. (default: 3)
  --confirm-seconds INTEGER
                           Wait specified seconds after transaction
                           confirmation. No waiting occurs if a non-positive
                           number of seconds is specified. The specified wait
                           period is randomly increased up to a factor of two.
                           Increasing this value will increase the probability
                           that the submitting node has seen the transactions
                           that Marlowe Runtime has seen. (default: 3)
  --retry-seconds INTEGER  Wait specified seconds after after a failed
                           transaction before trying again. No retries occur if
                           a non-positive number of seconds is specified.
                           (default: 10)
  --retry-limit INTEGER    Maximum number of attempts for trying a failed
                           transaction again. Each subsequent retry waits twice
                           as long as the previous retry. No retries occur if a
                           non-positive number of retries is specified.
                           (default: 5)
```
