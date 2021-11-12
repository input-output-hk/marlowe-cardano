{ name = "actus"
, dependencies =
  [ "arrays"
  , "const"
  , "datetime"
  , "effect"
  , "foreign"
  , "identity"
  , "maybe"
  , "prelude"
  , "profunctor-lenses"
  , "record"
  , "variant"
  ]
, packages = ../packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
