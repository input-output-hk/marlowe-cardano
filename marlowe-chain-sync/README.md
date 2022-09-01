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

To run the chain sync server, you can use the `run-chainseekd` command.
Alternatively, you can directly invoke `cabal run chainseekd` inside this
directory to provide custom command line parameters. The manual can be read by
running `./man docs/marlowe-chain-sync.1`.

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
