let upstream =
      https://github.com/input-output-hk/purescript-web-common/releases/download/v1.2.2/packages.dhall sha256:264991f1254aabddf9e1cfc83ea9c2ad356ebc3aa1f462a63d3087db09d5f3b7

let overrides = {=}

let additions = {=}

in  upstream // overrides // additions
