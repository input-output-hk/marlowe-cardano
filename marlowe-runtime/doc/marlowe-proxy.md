# Marlowe Sync

The `marlowe-proxy` executable provides a single unified public API for the whole runtime.

```console
marlowe-proxy : an API proxy service for the Marlowe Runtime.

Usage: marlowe-proxy [-h|--host HOST_NAME] [-p|--port PORT_NUMBER]
                     [--marlowe-sync-host HOST_NAME]
                     [--marlowe-sync-port PORT_NUMBER]
                     [--marlowe-header-port PORT_NUMBER]
                     [--marlowe-query-port PORT_NUMBER] [--tx-host HOST_NAME]
                     [--tx-command-port PORT_NUMBER]
                     [--log-config-file FILE_PATH]

  API proxy service for Marlowe Runtime

Available options:
  -h,--help                Show this help text
  -h,--host HOST_NAME      The host name to run the server on.
                           (default: "127.0.0.1")
  -p,--port PORT_NUMBER    The port number to run the server on. (default: 3700)
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
  --log-config-file FILE_PATH
                           The logging configuration JSON file.
```
