module Server (server,Config(..)) where

import Pure.Elm
import qualified Pure.Server as Server

import qualified Connection 

data Config = Config
  { host :: String, 
    port :: Int
  }

data Model = Model

data Message = Startup

server :: Config -> View
server = run (App [Startup] [] [] model update view) 
  where
    model = Model

    update Startup _ mdl = pure mdl

    view config _ = 
      Server.Server (host config) (port config) 
        (Connection.connection . Connection.Config)

