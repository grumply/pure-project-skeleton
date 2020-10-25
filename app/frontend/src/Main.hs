module Main where

import Pure.Elm 
import Pure.WebSocket

import App

main :: IO ()
main = do
  ws <- clientWS "127.0.0.1" 8081
  inject body (app (config ws))
  where
    config ws = Config { socket = ws }
