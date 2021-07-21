module App (App(..)) where

import Components.Hello
import Components.Home
import Components.Time

import Pure.Elm.Application hiding (Time,run)
import Pure.Elm.Component (run)
import Pure.WebSocket

import Shared

data App = App
  { socket :: WebSocket
  }

instance Application App where
  data Route App = HomeR | TimeR | HelloR
    
  home = HomeR

  location = \case
    HomeR  -> "/"
    TimeR  -> "/time"
    HelloR -> "/hello"

  routes = do
    path "/hello" do
      dispatch HelloR
    
    path "/time" do
      dispatch TimeR

    dispatch HomeR

  view rt App { socket } _ = case rt of
    HomeR  -> run Home
    HelloR -> run (Hello socket)
    TimeR  -> run (Time socket)