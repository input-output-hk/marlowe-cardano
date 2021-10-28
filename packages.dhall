let upstream =
      https://raw.githubusercontent.com/input-output-hk/plutus-apps/36eed44347266ac8762d344ba15116013523fe0d/packages.dhall sha256:dd0321dc883d589df013b9a985c662d4c0982ab24772086b509005530c91414f

let overrides = {=}

let additions = {=}

in  upstream // overrides // additions
