{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "marlowe-dashboard-client"
, dependencies =
  [ "aff-promise"
  , "avar"
  , "concurrent-queues"
  , "coroutines"
  , "debug"
  , "effect"
  , "filterable"
  , "formatters"
  , "halogen"
  , "json-helpers"
  , "markdown"
  , "node-fs"
  , "now"
  , "numerics"
  , "prelude"
  , "psci-support"
  , "remotedata"
  , "servant-support"
  , "test-unit"
  , "undefinable"
  , "unfoldable"
  , "uuid"
  , "web-common"
  , "web-socket"
  ]
, packages = ../packages.dhall
, sources =
  [ "src/**/*.purs"
  , "test/**/*.purs"
  , "generated/**/*.purs"
  , "${env:WEB_COMMON_SRC as Text}/**/*.purs"
  , "web-common-marlowe/**/*.purs"
  ]
}
