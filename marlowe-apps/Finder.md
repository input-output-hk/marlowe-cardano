# Find Active Marlowe Contracts

The command-line tool `marlowe-finder` watches a Cardano blockchain for contracts that are "active" (i.e., awaiting input). To run it, set environment variables to the hosts and ports for the Marlowe Runtime instances (see [Help](#help)) and filter the output for information of interest.


```bash
marlowe-finder |& jq 'select(.FinderProcess.fields.action == "wait") | .FinderProcess.fields | del(.action)'
```

```JSON
{
  "contractId": "22faecc06b460e18a92c628831410131007a988a77dec1562d31a0b8b78d937e#1",
  "transactionId": "22faecc06b460e18a92c628831410131007a988a77dec1562d31a0b8b78d937e"
}
```
```JSON
{
  "contractId": "04f5c00d32dad8d7b38502996d3a167728cbd21fd21592e38257705f48195bcf#1",
  "transactionId": "04f5c00d32dad8d7b38502996d3a167728cbd21fd21592e38257705f48195bcf"
}
```


## Help

```console
$ marlowe-finder --help

marlowe-finder : find active Marlowe contracts

Usage: marlowe-finder [--chain-sync-host HOST_NAME]
                      [--chain-sync-command-port PORT_NUMBER]
                      [--chain-sync-port PORT_NUMBER]
                      [--marlowe-sync-host HOST_NAME]
                      [--marlowe-sync-port PORT_NUMBER]
                      [--marlowe-header-port PORT_NUMBER]
                      [--marlowe-query-port PORT_NUMBER] [--tx-host HOST_NAME]
                      [--tx-command-port PORT_NUMBER]
                      [--timeout-seconds INTEGER] [--build-seconds INTEGER]
                      [--confirm-seconds INTEGER] [--retry-seconds INTEGER]
                      [--retry-limit INTEGER] [--polling SECONDS]
                      [--requeue SECONDS]

  This command-line tool watches the blockchain for Marlowe contracts for active
  Marlowe contracts.

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
  --polling SECONDS        The polling frequency for waiting on Marlowe Runtime.
  --requeue SECONDS        The requeuing frequency for reviewing the progress of
                           contracts on Marlowe Runtime.
```
