let upstream =
      https://github.com/input-output-hk/purescript-web-common/releases/download/v1.2.0/packages.dhall sha256:2fd872f5d3fbd13ffc3859dafa2bbd34df9d86874d1368207b5490db5532eff6

let overrides = {=}

let additions = {=}

in  upstream // overrides // additions
