
<a id='changelog-0.0.2'></a>
# 0.0.2 — 2023-06-15

## Added

* These endpoints `POST` handlers were extended so they return transaction as a result (instead of transaction body) if a particular header is present:

  * `contracts` endpoint uses "Accept: application/vendor.iog.marlowe-runtime.contract-tx-json" header

  * `transactions` endpoint uses "Accept: application/vendor.iog.marlowe-runtime.apply-inputs-tx-json" header

  * `withdraw` endpoint uses "Accept: application/vendor.iog.marlowe-runtime.withdraw-tx-json" header

* All the above endpoints accept also `witnessset` as a payload for `PUT` request now.
