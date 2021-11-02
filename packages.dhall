let upstream =
      https://github.com/input-output-hk/purescript-web-common/releases/download/v1.1.3/packages.dhall sha256:52dd98f851aa7b2f5285aa3802c6e976674f4035e4ef70853ec606011c56361c

let overrides = {=}

let additions = {=}

in  upstream // overrides // additions
