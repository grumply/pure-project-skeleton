let backend = ../config.dhall
      { name = "backend"
      , synopsis = "backend server"
      }
let deps =
      [ "base"
      , "pure-elm"
      , "pure-server"
      , "pure-websocket"
      , "shared"
      ]
in
  backend //
    { dependencies = deps
    , executables =
        { backend =
          { source-dirs = [ "src" ]
          , main = "Main.hs"
          , dependencies = deps
          } 
        }
    }