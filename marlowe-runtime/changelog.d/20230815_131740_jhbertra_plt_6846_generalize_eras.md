### Changed

- BREAKING: marlowe-tx: Create, ApplyInputs, and Withdraw now use the era reported by the node to build the TxBody.
  - Era is queried from the node at the time of job execution.
  - The era information is returned in the results of the jobs.
  - If the era does not support reference scripts (i.e. prior to Babbage) the
    command will fail wit an `EraUnsupported` error.
- BREAKING: marlowe-tx: Submit now accepts transactions from any era that
  supports reference scripts, not exclusively babbage. This is a breaking
  change because the era information must now be included in the command.
