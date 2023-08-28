### Changed

- BREAKING `Withdraw` now accepts a set of payout tx outs instead of a contract
  ID and a role token. The old behaviour can be emulated via a query to fetch
  unclaimed payouts for a contract.
