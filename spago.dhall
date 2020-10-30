{ name = "warren-codec"
, dependencies = 
  [ "arrays"
  , "effect"
  , "quickcheck"
  , "ordered-collections"
  , "psci-support"
  , "test-unit" 
  , "unicode"
  ]
, packages = ./packages.dhall
, sources = 
  [ "src/**/*.purs"
  , "test/**/*.purs" 
  ]
}
