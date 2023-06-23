### Fixed

- Metadata tag length cannot be longer than 64 characters
  - metadata tags now get chunked into lists of text of max. 64 characters
    each to comply with the CDDL specification for transaction metadata.
