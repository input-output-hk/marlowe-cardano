# Marlowe Run Server

There are 3 services that conform the backend for the Marlowe Run Dapp.

- The [Marlowe PAB](../marlowe/pab)
- An instance of the [Wallet-BackEnd](https://github.com/input-output-hk/cardano-wallet) ()
- This application

This application is both a web-server for the frontend in production (in development is served via webpack) and an API to simplify access to the Wallet BackEnd (WBE) and eventually the Metadata (not developed yet).

## Developing

In order to develop this application is recommended to be inside `nix develop` (read the project's [Readme](../README.adoc) for more information). Once inside, you can use

```bash
# To do a build
$ cabal build exe:marlowe-dashboard-server

# To build and run
$ cabal run marlowe-dashboard-server:marlowe-dashboard-server -- webserver -c ./config.json

# To have a fast compile loop
$ ghcid --command "cabal new-repl lib:marlowe-dashboard-server"

# You can run similar ide for the whole app (`ghcid` is going to crash if `lib` part does not compile)
$ ghcid --command "cabal new-repl exe:marlowe-dashboard-server"
```

## Configuring

This application is configured via a JSON file with the following structure (where `verbosity` is optional field ranging from 0 to 3):

```json
{
  "wbeConfig": { "host": "localhost", "port": 8090 },
  "chainIndexConfig": { "host": "localhost", "port": 9083 },
  "staticPath": "../marlowe-dashboard-client/static",
  "verbosity": 3
}
```
