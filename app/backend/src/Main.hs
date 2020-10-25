module Main where

import Pure.Elm
import Server

import Control.Monad

main :: IO ()
main = inject body (server config) >> keepalive
  where
    config = Config
      { host = "127.0.0.1",
        port = 8081
      }
      
    keepalive = forever do
      delay Minute