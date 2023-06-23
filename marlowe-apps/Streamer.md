# Streaming Marlowe Contracts

The command-line tool `marlowe-streamer` watches a Cardano blockchain for contracts' transactions, streaming them as [server-sent events (SSE)](https://en.wikipedia.org/wiki/Server-sent_events). To run it, set environment variables to the hosts and ports for the Marlowe Runtime instances (see [Help](#help)).

Start the server in one terminal:

```bash
marlowe-streamer
```

View the stream in a web browser at http://0.0.0.0:1564, or at the command line:

```bash
curl -N http://0.0.0.0:1564
```

```console
. . .

data:{"actions":[{"action":"notify"}],"block":355197,"contractId":"59ca20268f36b8e2aa423052d96b66ebf6e0770a693e44f6682938d938579978#1","slot":14400312,"transactionId":"febe3b90486f8a25582cd340a2b3193817c6909f783287c4ada2751d04eb7d80","value":null}

data:{"actions":["create"],"block":355217,"contractId":"36fa91e3d2ab8acc43886e07140a4ca4386eda82de272da86c89fd51c17643e7#1","slot":14400786,"transactionId":"36fa91e3d2ab8acc43886e07140a4ca4386eda82de272da86c89fd51c17643e7","value":{"ada":15000000,"tokens":[]}}

data:{"actions":[{"action":"choose","actor":"addr_test1vq9prvx8ufwutkwxx9cmmuuajaqmjqwujqlp9d8pvg6gupczgtm9j","choice":"Amount","number":1168390}],"block":355218,"contractId":"36fa91e3d2ab8acc43886e07140a4ca4386eda82de272da86c89fd51c17643e7#1","slot":14400866,"transa
ctionId":"ae6429d79962180f9d91036eef960244fc86bc9f9566c9a3a0d138818ed74f9f","value":{"ada":15000000,"tokens":[]}}

data:{"actions":[{"account":"addr_test1vq9prvx8ufwutkwxx9cmmuuajaqmjqwujqlp9d8pvg6gupczgtm9j","action":"deposit","actor":"addr_test1vq9prvx8ufwutkwxx9cmmuuajaqmjqwujqlp9d8pvg6gupczgtm9j","amount":1168390,"token":""}],"block":355220,"contractId":"36fa91e3d2ab8acc43886
e07140a4ca4386eda82de272da86c89fd51c17643e7#1","slot":14400898,"transactionId":"670a7a9f5684ed098cda328bc6c4a3dbcd6f687d701ce5a7d486cdeefaeba937","value":{"ada":16168390,"tokens":[]}}

data:{"actions":[{"action":"notify"}],"block":355221,"contractId":"36fa91e3d2ab8acc43886e07140a4ca4386eda82de272da86c89fd51c17643e7#1","slot":14400913,"transactionId":"e3529fb4e49ce72d047af682bec7caed0e59cc63cf43cebd1a21f7e1234b100a","value":null}

data:{"actions":["create"],"block":355238,"contractId":"c935c013aa6370db410542abefca2ff9435030a15f9a56bfa09a85131d8e8905#1","slot":14401128,"transactionId":"c935c013aa6370db410542abefca2ff9435030a15f9a56bfa09a85131d8e8905","value":{"ada":15000000,"tokens":[]}}

data:{"actions":[],"block":458953,"contractId":"c935c013aa6370db410542abefca2ff9435030a15f9a56bfa09a85131d8e8905#1","slot":16673931,"transactionId":"77d273ea2620f25378b2dbb18c51e07314da3e2cc99d2fbd2b9c9a08008a81ae","value":null}

data:{"actions":["create"],"block":355243,"contractId":"0014afa55bcfa78e41c8312a34fab4261c98565a37f1a0d4c89a1f75bcc5c687#1","slot":14401271,"transactionId":"0014afa55bcfa78e41c8312a34fab4261c98565a37f1a0d4c89a1f75bcc5c687","value":{"ada":15000000,"tokens":[]}}

data:{"actions":[{"action":"choose","actor":"addr_test1vq9prvx8ufwutkwxx9cmmuuajaqmjqwujqlp9d8pvg6gupczgtm9j","choice":"Amount","number":1323220}],"block":355244,"contractId":"0014afa55bcfa78e41c8312a34fab4261c98565a37f1a0d4c89a1f75bcc5c687#1","slot":14401310,"transa
ctionId":"c7aebde2168c7167ef917ec82b93ac216a81d63aa8205fb03b01ac417ba048fa","value":{"ada":15000000,"tokens":[]}}

. . .
```


## Help

```console
$ marlowe-streamer --help

marlowe-streamer : stream all Marlowe contracts via HTTP SSE

Usage: marlowe-streamer [--chain-sync-host HOST_NAME]
                        [--chain-sync-port PORT_NUMBER]
                        [--chain-sync-command-port PORT_NUMBER]
                        [--marlowe-runtime-host HOST_NAME]
                        [--marlowe-runtime-port PORT_NUMBER]
                        [--timeout-seconds INTEGER] [--build-seconds INTEGER]
                        [--confirm-seconds INTEGER] [--retry-seconds INTEGER]
                        [--retry-limit INTEGER] [--polling SECONDS]
                        [--requeue SECONDS] [--end-at-tip] [--origin HOST]
                        [--port PORT] [--verbosity Terse|Standard|Verbose]

  This command-line tool watches the blockchain for Marlowe contracts for
  Marlowe contracts and streams them as server-sent events.

Available options:
  -h,--help                Show this help text
  --chain-sync-host HOST_NAME
                           The hostname of the Marlowe Runtime chain-sync
                           server. Can be set as the environment variable
                           MARLOWE_CHAIN_SYNC_HOST (default: "0.0.0.0")
  --chain-sync-port PORT_NUMBER
                           The port number of the chain-sync server's
                           synchronization API. Can be set as the environment
                           variable MARLOWE_CHAIN_SYNC_PORT (default: 13715)
  --chain-sync-command-port PORT_NUMBER
                           The port number of the chain-sync server's job API.
                           Can be set as the environment variable
                           MARLOWE_CHAIN_SYNC_COMMAND_PORT (default: 13720)
  --marlowe-runtime-host HOST_NAME
                           The hostname of the Marlowe Runtime server. Can be
                           set as the environment variable MARLOWE_RT_HOST
                           (default: "0.0.0.0")
  --marlowe-runtime-port PORT_NUMBER
                           The port number of the Marlowe Runtime server. Can be
                           set as the environment variable MARLOWE_RT_PORT
                           (default: 13700)
  --timeout-seconds INTEGER
                           Timeout in seconds for transaction confirmation.
                           (default: 900)
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
                           (default: 5)
  --requeue SECONDS        The requeuing frequency for reviewing the progress of
                           contracts on Marlowe Runtime. (default: 20)
  --end-at-tip             Stop the process when the tip of all contracts has
                           been reached.
  --origin HOST            Value for Access-Control-Allow-Origin
                           (default: "0.0.0.0")
  --port PORT              Port number for streaming data. (default: 1564)
  --verbosity Terse|Standard|Verbose
                           The verbosity of the output data stream.
                           (default: Terse)
```
