let mkPackage =
      https://raw.githubusercontent.com/purescript/package-sets/psc-0.13.0-20190626/src/mkPackage.dhall sha256:0b197efa1d397ace6eb46b243ff2d73a3da5638d8d0ac8473e8e4a8fc528cf57

let upstream =
      https://github.com/input-output-hk/purescript-web-common/releases/download/v1.2.2/packages.dhall sha256:264991f1254aabddf9e1cfc83ea9c2ad356ebc3aa1f462a63d3087db09d5f3b7
in  upstream
  with
    moldy = mkPackage
      [ "prelude", "foldable-traversable", "strings"
      , "console", "psci-support"
      ]
      "https://github.com/liamgoodacre/purescript-moldy.git"
      "v3.0.0"
  with
    polyform = mkPackage
      [ "arrays", "bifunctors", "control", "effect", "either"
      , "enums", "functors", "identity", "invariant", "lists"
      , "maybe", "newtype", "parallel", "partial", "prelude"
      , "profunctor", "psci-support", "quickcheck", "quickcheck-laws"
      , "record", "transformers", "tuples", "typelevel-prelude"
      , "unsafe-coerce", "validation", "variant"
      ]
      "https://github.com/purescript-polyform/polyform"
      "d177fa5e04a29babf0f86cf57561ea6bf2317c36"
  with
    typelevel-eval = mkPackage
      [ "console", "effect", "leibniz", "psci-support", "record" ]
      "https://github.com/natefaubion/purescript-typelevel-eval"
      "v0.5.0"
