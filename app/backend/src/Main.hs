module Main where

import Pure.Elm.Component
import Server

import Control.Monad

main :: IO ()
main = inject body (run server) >> keepalive
  where
    server = Server
      { host = "127.0.0.1",
        port = 8081
      }
      
    keepalive = forever do
      delay Minute