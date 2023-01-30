# Marlowe Sync

The `marlowe-sync` executable provides protocols that access Marlowe contract information.

```console
marlowe-sync : a contract synchronization and query service for the Marlowe
Runtime.

Usage: marlowe-sync (-d|--database-uri DATABASE_URI) [--sync-port PORT_NUMBER] 
                    [--header-sync-port PORT_NUMBER] [-h|--host HOST_NAME] 
                    [--log-config-file FILE_PATH]

  Contract synchronization and query service for Marlowe Runtime

Available options:
  -h,--help                Show this help text
  -d,--database-uri DATABASE_URI
                           URI of the database where the contract information is
                           saved.
  --sync-port PORT_NUMBER  The port number to run the sync protocol on.
                           (default: 3724)
  --header-sync-port PORT_NUMBER
                           The port number to run the header sync protocol on.
                           (default: 3725)
  -h,--host HOST_NAME      The host name to run the server on.
                           (default: "127.0.0.1")
  --log-config-file FILE_PATH
                           The logging configuration JSON file.

```
