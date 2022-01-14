let upstream =
      https://github.com/input-output-hk/purescript-web-common/releases/download/v1.2.2/packages.dhall sha256:264991f1254aabddf9e1cfc83ea9c2ad356ebc3aa1f462a63d3087db09d5f3b7

let overrides = {=}

let additions =
      { polyform =
        { dependencies =
          [ "arrays"
          , "bifunctors"
          , "control"
          , "effect"
          , "either"
          , "enums"
          , "functors"
          , "identity"
          , "invariant"
          , "lists"
          , "maybe"
          , "newtype"
          , "parallel"
          , "partial"
          , "prelude"
          , "profunctor"
          , "psci-support"
          , "quickcheck"
          , "quickcheck-laws"
          , "record"
          , "transformers"
          , "tuples"
          , "typelevel-prelude"
          , "unsafe-coerce"
          , "validation"
          , "variant"
          ]
        , repo = "https://github.com/purescript-polyform/polyform"
        , version = "d177fa5e04a29babf0f86cf57561ea6bf2317c36"
        }
      }

in  upstream // overrides // additions
