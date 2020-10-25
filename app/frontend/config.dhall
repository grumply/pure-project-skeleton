let frontend = ../config.dhall
      { name = "frontend"
      , synopsis = "frontend client" 
      }
let deps = 
      [ "base"
      , "pure-elm"
      , "pure-websocket"
      , "shared"
      ]
in
  frontend //
    { dependencies = deps
    , executables =
        { frontend =
          { source-dirs = [ "src" ]
          , main = "Main.hs"
          , dependencies = deps
          } 
        }
    }
