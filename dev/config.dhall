let dev = ../config.dhall
      { name = "dev"
      , synopsis = "dev environment" 
      }
in
  dev //
    { dependencies = 
        [ "base"
        , "pure"
        , "pure-elm"
        , "pure-time"
        , "pure-txt"
        , "pure-txt-interpolate"
        , "optparse-applicative"
        , "fsnotify"
        , "directory"
        , "filepath"
        , "process"
        , "containers"
        ]
    , executables = 
        { dev = 
          { source-dirs = [ "src" ]
          , main = "Main.hs" 
          } 
        }
    }
