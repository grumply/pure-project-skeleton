let test = ../config.dhall
      { name = "test"
      , synopsis = "executable tests"
      }

in
  test //
    { dependencies = 
        (../shared/config.dhall).dependencies 
        # (../frontend/config.dhall).dependencies 
        # (../backend/config.dhall).dependencies
        # [ "pure-test"
          , "pure-bench" 
          , "frontend"
          , "shared"
          , "backend"
          ]
    , executables = 
        { test = 
          { source-dirs = [ "src" ] 
          , main = "Main.hs" 
          , other-modules = [] : List Text
          }
        }
    }

