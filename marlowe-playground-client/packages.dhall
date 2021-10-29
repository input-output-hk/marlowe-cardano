let upstream =
      ../packages.dhall

let overrides = {=}

let additions =
      { numerics =
          { dependencies =
            [ "prelude", "integers", "rationals", "uint", "bigints" ]
          , repo = "https://github.com/Proclivis/purescript-numerics"
          , version = "v0.1.2"
          }
      , datetime-iso =
          { dependencies =
            [ "argonaut-codecs", "datetime", "parsing", "newtype" ]
          , repo = "https://github.com/jmackie/purescript-datetime-iso"
          , version = "v4.0.0"
          }
      }

in  upstream ⫽ overrides ⫽ additions
