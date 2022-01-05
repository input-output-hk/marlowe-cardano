{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "marlowe-playground-client"
, dependencies =
  [ "aff"
  , "aff-promise"
  , "affjax"
  , "argonaut"
  , "argonaut-codecs"
  , "argonaut-core"
  , "arrays"
  , "bifunctors"
  , "console"
  , "control"
  , "datetime"
  , "datetime-iso"
  , "decimals"
  , "dom-indexed"
  , "effect"
  , "either"
  , "enums"
  , "exceptions"
  , "filterable"
  , "foldable-traversable"
  , "foreign"
  , "foreign-generic"
  , "foreign-object"
  , "formatters"
  , "functions"
  , "gen"
  , "halogen"
  , "halogen-hooks"
  , "halogen-subscriptions"
  , "http-methods"
  , "integers"
  , "js-timers"
  , "json-helpers"
  , "lists"
  , "maybe"
  , "newtype"
  , "node-buffer"
  , "node-fs"
  , "nonempty"
  , "numbers"
  , "ordered-collections"
  , "partial"
  , "prelude"
  , "profunctor"
  , "profunctor-lenses"
  , "psci-support"
  , "quickcheck"
  , "record"
  , "refs"
  , "remotedata"
  , "routing"
  , "routing-duplex"
  , "servant-support"
  , "simple-json"
  , "spec"
  , "spec-quickcheck"
  , "strings"
  , "tailrec"
  , "transformers"
  , "tuples"
  , "type-equality"
  , "unfoldable"
  , "web-common"
  , "web-dom"
  , "web-events"
  , "web-html"
  , "web-socket"
  , "web-uievents"
  ]
, packages = ../packages.dhall
, sources =
  [ "src/**/*.purs"
  , "test/**/*.purs"
  , "generated/**/*.purs"
  , "../web-common-marlowe/src/**/*.purs"
  , "${env:WEB_COMMON_PLAYGROUND_SRC as Text}/src/**/*.purs"
  ]
}
