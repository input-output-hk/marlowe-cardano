# Marlowe Chain Sync

An efficient chain indexer and synchronization engine for the Marlowe Runtime.

### Running the Cardano Node

You can use the provided command `start-cardano-node` to run a local node that connects
to `testnet`. It will expose its socket file at `/tmp/node.socket`. It will
save its database in `<marlowe-cardano-repo-root>/db`.

### Running the chain sync

A prerequisite for running the chain sync process is a migrated PostgreSQL
instance. The repo root folder provides a `docker-compose.yaml` which you can use to run
a PostgreSQL server using `docker-compose up`. To migrate the database, you
need to run `sqitch deploy` from within the nix shell in this directory. The
shell includes the correct environment variables to run `sqitch` against the
postgres container spawned by `docker-compose up`. In brief:

```sh
$ nix-shell          # If you are not already in the nix shell
$ docker-compose up  # To start the PostgreSQL server
$ cd marlowe-chain-sync
$ sqitch deploy      # To migrate the database.
```

You should run `sqitch deploy` whenever you pull new commits, as there may be
new migrations that need to be run. For a detailed guide to writing and
managing migrations, please read https://sqitch.org/docs/manual/sqitchtutorial/.

The Marlowe chain sync consists of two processes: `marlowe-chain-indexer`, and
`marlowe-chain-sync`. This split is for scalability purposes. `marlowe-chain-indexer`
copies blocks and transactions into the database and `marlowe-chain-sync` serves the
Chain Sync Protocol, backed by the database.

To run the chain indexer process, you can use `nix run .#marlowe-chain-indexer`.
Alternatively, you can directly invoke `cabal run marlowe-chain-indexer`.

To run the chain sync server, you can use `nix run .#marlowe-chain-sync`.
Alternatively, you can directly invoke `cabal run marlowe-chain-sync`.

### Adding a new query

In order to add support for a new query, the following steps need to be
performed:

- A new constructor needs to be added to `Language.Marlowe.ChainSync.Api.Move`.
  The constructor should specify the parameters needed to execute the query,
  the error type for the query (or `Void` if no error can occur), and the
  result type.
- The codec needs to be updated (specifically `putMove`, `getMove`, `putResult`,
  `getResult`, `putError` and `getError`). This involves selecting a `tag`
  value to represent this constructor.
- An SQL query needs to be written and added to
  `Language.Marlowe.Runtime.ChainSync.Database.PostgreSQL.performMove` that
  will perform the actual query. It needs to check and enforce error conditions,
  find the result from the client's position, and retrieve the new position for
  the client.

### Configuring the log output

The log output of `marlowe-chain-sync` can be configured on-the-fly (i.e. updated while
it is running) using a config file. This file is passed via an optional command line
argument (see `--help`). An example config file is located in this directory.
It shows the default configuration.

Each key in the configuration object corresponds to an event type. Setting the
value to true means the event will be logged, false means it won't be logged,
and not present means the default behaviour will be used. For example, to turn
on the `chain-sync.send` event (disabled by default), you can use a config file
with contents.

```json
{
  "chain-sync.send": true
}
```

You can also assign an object value to each key. The keys of this object
correspond to fields associated with that event. `true` means include the
field, `false` means don't include the field, and not defined means the default
behaviour is used. For example, to log the `stateBefore` field of the
`query.recv` event (disabled by default), you can use a config file with the contents:

```json
{
  "query.recv": {
    "stateBefore": true
  }
}
```

Each time you save this file, it will be read and the configuration will be
updated by `marlowe-chain-sync`. If it cannot find the config file, the default
configuration will be used.
