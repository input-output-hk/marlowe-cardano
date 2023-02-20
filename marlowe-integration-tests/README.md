# Marlowe integration tests

The Marlowe integration tests are a suite of tests that verify the correct behavior of Marlowe on a local cluster. In order to run the tests, first make sure you check the prerequisites described in [the README.md of the marlowe-integration framework](../marlowe-integration/README.md).

## Running the integration tests

To run the integration tests, you can use the following commands:

```bash
cd marlowe-cardano
cabal run marlowe-integration-tests
```

This command runs all the integration tests. If you want to run only a specific test or set of tests, you can use the `--match` option to filter the tests by name or pattern. For example, to run only the `Basic e2e scenario` test, you can use the following command:

```bash
cabal run marlowe-integration-tests -- --match "/Language.Marlowe.Runtime.Integration/Marlowe runtime API/Basic e2e scenario/"
```

This command runs only the test that matches the specified pattern.

## Troubleshooting

If you encounter issues with the PostgreSQL database or the Marlowe integration tests, here are some tips to help you troubleshoot:

- Make sure the PostgreSQL service is running and listening on the correct port (`5432` by default).
- Check the `pg_hba.conf` file to make sure it allows connections from the local host using the `postgres` user with no password.
- Make sure the template database exists, by default it is `template1`.
- Make sure you are running the integration tests from the `marlowe-cardano` directory, not `marlowe-cardano/marlowe-integration-tests`.
- If you see error messages about the PostgreSQL database rejecting connections, check the PostgreSQL logs for clues.
