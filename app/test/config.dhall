let test = ../config.dhall
      { name = "test"
      , synopsis = "executable tests"
      }
let deps = 
      (../shared/config.dhall).dependencies 
        # (../frontend/config.dhall).dependencies 
        # (../backend/config.dhall).dependencies
        # [ "pure-test"
          , "pure-bench" 
          , "frontend"
          , "shared"
          , "backend"
          ]
in
  test //
    { dependencies = deps
    , library =
        { source-dirs = [ "src" ]
        , other-modules = [] : List Text
        }
    , executables = 
        { test = 
          { source-dirs = [ "src" ] 
          , main = "Main.hs" 
          , dependencies = [ "test" ] # deps
          }
        }
    }

