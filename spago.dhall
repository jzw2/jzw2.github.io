{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
  [ "console"
  , "control"
  , "effect"
  , "foldable-traversable"
  , "foreign"
  , "halogen"
  , "prelude"
  , "psci-support"
  , "routing"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
