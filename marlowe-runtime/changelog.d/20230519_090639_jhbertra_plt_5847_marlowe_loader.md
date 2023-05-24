<!--
A new scriv changelog fragment.

Uncomment the section that is right (remove the HTML comment wrapper).
-->

<!--
### Removed

- A bullet item for the Removed category.

-->

### Added

- `marlowe-contract` service for contract definition storage.
    - Includes a file-based store and a server for the `MarloweLoad` protocol.
- BREAKING `MarloweLoad` as a sub-protocol of `MarloweRuntime` service
    - `marlowe-proxy` now requires a host and port for `marlowe-contract`
- functions for running `MarloweLoad` clients to `marlowe-client`
- `load` command to `marlowe-runtime-cli`

<!--
### Changed

- A bullet item for the Changed category.

-->
<!--
### Deprecated

- A bullet item for the Deprecated category.

-->
<!--
### Fixed

- A bullet item for the Fixed category.

-->
<!--
### Security

- A bullet item for the Security category.

-->
