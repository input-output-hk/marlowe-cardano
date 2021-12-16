let upstream =
      https://github.com/input-output-hk/purescript-web-common/releases/download/v1.1.5/packages.dhall sha256:ddf79f54f80dbdaec6c7bce0bae4e583b3b035c9c9bfc7affdb10cc81c96b0f9

let overrides = {=}

let additions = {=}

in  upstream // overrides // additions
