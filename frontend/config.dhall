let frontend = ../config.dhall
      { name = "frontend"
      , synopsis = "frontend client" 
      }
in 
  frontend //
    { dependencies = 
        [ "base"
        , "pure"
        , "pure-websocket"
        , "shared" 
        ]
    , library =
        { source-dirs = [ "src" ]
        }
    , executables = 
        { frontend = 
          { source-dirs = [ "src" ]
          , main = "Main.hs" 
          } 
        }
    }
