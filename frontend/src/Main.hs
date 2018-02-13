{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Pure.App as App
import Pure.View

main :: IO ()
main = App.run App.App {..}
  where
    key    = "MyApp"
    build  = return
    prime  = return ()
    root   = Nothing
    routes = App.dispatch HomeR
    pages HomeR = pure (App.partial _MyApp)

data MyAppRoute = HomeR deriving Eq

_MyApp = Controller {..}
  where
    key    = "MyApp"
    build  = return
    prime  = return ()
    model  = Const ()
    view _ = Div [] "My App"