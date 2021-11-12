{ name = "actus"
, dependencies =
  [ "arrays"
  , "const"
  , "effect"
  , "foreign"
  , "identity"
  , "maybe"
  , "prelude"
  , "profunctor-lenses"
  , "record"
  ]
, packages = ../packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
