let backend = ../config.dhall
      { name = "backend"
      , synopsis = "backend server" 
      }
in
  backend //
    { dependencies = 
        [ "base"
        , "pure"
        , "pure-elm"
        , "pure-server"
        , "pure-websocket"
        , "shared" 
        , "bytestring"
        , "containers"
        , "directory"
        , "filepath"
        ]
    , library =
        { source-dirs = [ "src" ]
        }
    , executables = 
        { backend = 
          { source-dirs = [ "src" ]
          , main = "Main.hs" 
          } 
        }
    }
