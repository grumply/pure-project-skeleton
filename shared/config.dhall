let shared = ../config.dhall
      { name = "shared"
      , synopsis = "shared types and apis" 
      }

in
  shared //
    { dependencies =
        [ "base"
        , "pure"
        , "pure-websocket"
        ]
    , library = 
        { source-dirs = ["src"] 
        }
    }
