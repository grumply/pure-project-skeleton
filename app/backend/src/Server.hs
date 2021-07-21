module Server (Server(..)) where

import Pure.Elm.Component hiding (Start,start)
import qualified Pure.Server as Pure

import Connection 

data Server = Server
  { host :: String, 
    port :: Int
  }

instance Component Server where
  data Msg Server = Start

  startup = [Start]

  upon = \case
    Start -> start

  view Server { host, port } _ = 
    Pure.Server host port (run . Connection)

start :: Update Server
start _ mdl = pure mdl