{ name = "affjax"
, dependencies =
  [ "aff"
  , "argonaut-core"
  , "arraybuffer-types"
  , "console"
  , "effect"
  , "foreign"
  , "form-urlencoded"
  , "http-methods"
  , "integers"
  , "math"
  , "media-types"
  , "nullable"
  , "psci-support"
  , "refs"
  , "unsafe-coerce"
  , "web-xhr"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
