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

Usage: marlowe-finder [--chain-seek-host HOST_NAME]
                      [--chain-seek-command-port PORT_NUMBER]
                      [--chain-seek-query-port PORT_NUMBER]
                      [--chain-seek-sync-port PORT_NUMBER]
                      [--history-host HOST_NAME]
                      [--history-command-port PORT_NUMBER]
                      [--history-query-port PORT_NUMBER]
                      [--history-sync-port PORT_NUMBER]
                      [--discovery-host HOST_NAME]
                      [--discovery-query-port PORT_NUMBER]
                      [--discovery-sync-port PORT_NUMBER] [--tx-host HOST_NAME]
                      [--tx-command-port PORT_NUMBER]
                      [--timeout-seconds INTEGER] [--polling SECONDS]
                      [--requeue SECONDS]

  This command-line tool watches the blockchain for Marlowe contracts for active
  Marlowe contracts.

Available options:
  -h,--help                Show this help text
  --chain-seek-host HOST_NAME
                           The hostname of the Marlowe Runtime chain-seek
                           server. Can be set as the environment variable
                           MARLOWE_RT_CHAINSEEK_HOST (default: "127.0.0.1")
  --chain-seek-command-port PORT_NUMBER
                           The port number of the chain-seek server's job API.
                           Can be set as the environment variable
                           MARLOWE_RT_CHAINSEEK_COMMAND_PORT (default: 13720)
  --chain-seek-query-port PORT_NUMBER
                           The port number of the chain-seek server's query API.
                           Can be set as the environment variable
                           MARLOWE_RT_CHAINSEEK_QUERY_PORT (default: 13716)
  --chain-seek-sync-port PORT_NUMBER
                           The port number of the chain-seek server's
                           synchronization API. Can be set as the environment
                           variable MARLOWE_RT_CHAINSEEK_SYNC_PORT
                           (default: 13715)
  --history-host HOST_NAME The hostname of the Marlowe Runtime history server.
                           Can be set as the environment variable
                           MARLOWE_RT_HISTORY_HOST (default: "127.0.0.1")
  --history-command-port PORT_NUMBER
                           The port number of the history server's job API. Can
                           be set as the environment variable
                           MARLOWE_RT_HISTORY_COMMAND_PORT (default: 13717)
  --history-query-port PORT_NUMBER
                           The port number of the history server's query API.
                           Can be set as the environment variable
                           MARLOWE_RT_HISTORY_QUERY_PORT (default: 13718)
  --history-sync-port PORT_NUMBER
                           The port number of the history server's
                           synchronization API. Can be set as the environment
                           variable MARLOWE_RT_HISTORY_SYNC_PORT
                           (default: 13719)
  --discovery-host HOST_NAME
                           The hostname of the Marlowe Runtime discovery server.
                           Can be set as the environment variable
                           MARLOWE_RT_DISCOVERY_HOST (default: "127.0.0.1")
  --discovery-query-port PORT_NUMBER
                           The port number of the discovery server's query API.
                           Can be set as the environment variable
                           MARLOWE_RT_DISCOVERY_QUERY_PORT (default: 13721)
  --discovery-sync-port PORT_NUMBER
                           The port number of the discovery server's
                           synchronization API. Can be set as the environment
                           variable MARLOWE_RT_DISCOVERY_SYNC_PORT
                           (default: 13722)
  --tx-host HOST_NAME      The hostname of the Marlowe Runtime transaction
                           server. Can be set as the environment variable
                           MARLOWE_RT_TX_HOST (default: "127.0.0.1")
  --tx-command-port PORT_NUMBER
                           The port number of the transaction server's job API.
                           Can be set as the environment variable
                           MARLOWE_RT_TX_COMMAND_PORT (default: 13723)
  --timeout-seconds INTEGER
                           Time timeout in seconds for transaction confirmation.
  --polling SECONDS        The polling frequency for waiting on Marlowe Runtime.
  --requeue SECONDS        The requeuing frequency for reviewing the progress of
                           contracts on Marlowe Runtime.
```
