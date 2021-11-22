{ name = "actus"
, dependencies =
  [ "datetime"
  , "effect"
  , "heterogeneous-collections"
  , "maybe"
  , "ordered-collections"
  , "prelude"
  , "these"
  , "variant"
  ]
, packages = ../packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
