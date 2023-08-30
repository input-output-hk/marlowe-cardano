### Added

- New filters to `GetHeaders` query to allow filtering contract headers by party
  (addresses and role tokens). Specifying parties in the filter will limit
  results to contracts which (visibly) contain either the address or role token as a
  party.
  - Regarding "visibly contain": this applies to merkleized contract. A
    merkleized contract visibly contains a party if the party can be extracted
    from its on-chain history without having access to the continuations. This
    means any parties contained in unexecuted paths of the contract are not
    included, because they aren't visible.
- New query parameters for `/contracts` endpoint `partyAddress` and `partyRole`.
  These give REST API access to the party filtering functionality mentioned
  above.
