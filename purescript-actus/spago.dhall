{ name = "actus"
, dependencies =
  [ "datetime"
  , "effect"
  , "enums"
  , "heterogeneous-collections"
  , "maybe"
  , "newtype"
  , "ordered-collections"
  , "prelude"
  , "these"
  , "variant"
  ]
, packages = ../packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
