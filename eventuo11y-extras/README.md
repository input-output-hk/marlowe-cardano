# eventuo11y-extras

Observability utilities built on top of eventu011y.

## Logging component

`Observe.Event.Component` exports a `logger` component (from `async-components`)
that exposes an `EventBackend` which will write events to a queue. The `logger`
writes the serialized events as log messages. Individual events can be enabled
and disabled by way of a configuration file.

The format of the configuration file is a JSON `object` with the following
schema:

```typescript
interface LogConfigMap {
  [eventKey: string]: bool | null | { [fieldKey: string]: bool };
}
```

For example:

```jsonc
{
  "event1": true,    // Enables the event with key `event1`. Uses default config for fields
  "event2": false,   // Disables the event with key `event2`.
  "event3": null,    // Uses the default config for `event3`. This is not necessary, as missing fields will use the default config.
  "event4": {        // Enables `event4`. Uses a custom config for fields.
    "field1": true,  // Enables `field1` for `event4`.
    "field2": false, // Disables `field2` for `event4`.
    "field3": true   // Uses the default config for `field3`. This is not necessary, missing fields will use the default config.
  }
}
```

Note that comments are not allowed in the actual log config file.

This file can be changed at runtime and the `logger` component will reload the
config automatically to apply the changes.

Though this functionality is not built-in to the `logger`, all applications in
`marlowe-chain-sync` and `marlowe-runtime` which use a `logger` have a
command-line option `--print-log-config` which will print the available events
with their default configuration to stdout. Note that field keys are not yet
supported in this feature. The intended use for this is to generate a log
config file:

```bash
# Generate a log config file with the default values
$ marlowe-sync --print-log-config > marlowe-sync.log.config
# Run marlowe-sync with the generated log config. You can enable and disable
# events by editing the file and the output of `marlowe-sync` will be changed to
# reflect the new log config.
$ marlowe-sync --log-config-file marlowe-sync.log.config
```
