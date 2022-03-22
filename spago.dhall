{ name = "affjax"
, dependencies =
  [ "aff"
  , "argonaut-core"
  , "arraybuffer-types"
  , "arrays"
  , "console"
  , "control"
  , "datetime"
  , "effect"
  , "either"
  , "exceptions"
  , "foldable-traversable"
  , "foreign"
  , "foreign-object"
  , "form-urlencoded"
  , "functions"
  , "http-methods"
  , "lists"
  , "maybe"
  , "media-types"
  , "newtype"
  , "nullable"
  , "prelude"
  , "transformers"
  , "web-dom"
  , "web-file"
  , "web-xhr"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
