# Application for Scale Testing of Marlowe Runtime

To run multiple contracts for multiple wallets, set environment variables to the hosts and ports for the Marlowe Runtime instances (see [Help](#help)), and on the command line supply that along with the number of repetitions and the pairs of addresses and keys.

```bash
marlowe-scaling 2 \
  addr_test1qryrafsnj3wt3as5pgeng8ddh42gq0dk8gphkz3mx8utzn6h3execksjs6h7k77qflc3mydgdlk98snlj6ngqzltl8mqjuqmk9=alice.skey \
  addr_test1qzl43spe69knxgfl5eqxrr89lwkef3elskmapjvzmy6akmu68l4aw87t9a9304rgj2p67tlrzaszh32ej3nlwp5t8zsqdcz20w=bob.skey \
|& jq 'select(.Contract)'
```

The output will show which transactions are submitted for which contracts, punctuated by an success/failure (`Right`/`Left`) report after each contract closes.

```JSON
{
  "Contract": {
    "end": "2022-12-31T16:16:28.399591192Z",
    "event-id": "f293b084-9b8c-45f0-8c1b-eaf12e0c57f0",
    "fields": {
      "success": "7741671ad473c5c1a532f921da6e8b9c942cc445ba316c70f0cf0dc395502ef6#1",
      "threadId": "ThreadId 2"
    },
    "start": "2022-12-31T16:15:34.313226613Z"
  }
}
```
```JSON
{
  "Contract": {
    "end": "2022-12-31T16:16:28.400022128Z",
    "event-id": "f70754c7-39b0-4634-a210-1e109e1c1047",
    "fields": {
      "success": "bcab6da80765cc128be6611224895c495d43a4a40ccf46fa3dd73a9a71e44c17#1",
      "threadId": "ThreadId 5"
    },
    "start": "2022-12-31T16:15:34.313234766Z"
  }
}
```
```JSON
{
  "Contract": {
    "end": "2022-12-31T16:17:22.765717631Z",
    "event-id": "a187c5f9-a740-49cb-8817-7c237593480b",
    "fields": {
      "success": "dd64edd77fc7405112b147ab847f9fc81952d1ab90bc6808c7851f84f84597bb#1",
      "threadId": "ThreadId 5"
    },
    "start": "2022-12-31T16:16:28.40002814Z"
  }
}
```
```JSON
{
  "Contract": {
    "end": "2022-12-31T16:17:23.322001857Z",
    "event-id": "6b808865-a785-4831-b83a-83dc63f48805",
    "fields": {
      "success": "8e8c216bf4a91e45c6da4f3c75211145e5fde662e12e7a93536d557c2d6248af#1",
      "threadId": "ThreadId 2"
    },
    "start": "2022-12-31T16:16:28.399615255Z"
  }
}

```

## Help

```console
$ marlowe-scaling --help

marlowe-scaling : run multiple Marlowe test contracts in parallel

Usage: marlowe-scaling [--chain-sync-host HOST_NAME]
                       [--chain-sync-command-port PORT_NUMBER]
                       [--chain-sync-port PORT_NUMBER]
                       [--marlowe-sync-host HOST_NAME]
                       [--marlowe-sync-port PORT_NUMBER]
                       [--marlowe-header-port PORT_NUMBER]
                       [--marlowe-query-port PORT_NUMBER] [--tx-host HOST_NAME]
                       [--tx-command-port PORT_NUMBER]
                       [--timeout-seconds INTEGER] [--build-seconds INTEGER]
                       [--confirm-seconds INTEGER] [--retry-seconds INTEGER]
                       [--retry-limit INTEGER] NATURAL [ADDRESS=KEYFILE]

  This command-line tool is a scaling test client for Marlowe Runtime: it runs
  multiple contracts in parallel against a Marlowe Runtime backend, with a
  specified number of contracts run in sequence for each party and each party
  running contracts in parallel.

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
  NATURAL                  The number of contracts to run sequentially for each
                           party.
  ADDRESS=KEYFILE          The addresses of the parties and the files with their
                           signing keys.
```
