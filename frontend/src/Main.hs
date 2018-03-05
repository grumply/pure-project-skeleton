{-# LANGUAGE OverloadedStrings, TemplateHaskell, NoMonomorphismRestriction #-}
module Main where

import qualified Pure.App as App
import Pure.View
import Pure.Render

import Lib

main :: IO ()
main =
  let ?serverIp = "127.0.0.1"
      ?serverPort = 8000
  in App.run app

app = App.App {..}
  where
    key    = "MyApp"
    build  = return
    prime  = return ()
    root   = Nothing
    routes = App.dispatch HomeR
    pages HomeR = pure (App.partial _MyApp)

data MyAppRoute = HomeR deriving Eq

data MyAppState ms = MyAppState Millis

_MyApp = Controller {..}
  where
    key    = "MyApp"
    build  = return
    prime  = return ()
    model  = MyAppState 0
    view (MyAppState (Millis ms)) =
      Div [ ]
        [ Button [ OnClick def sendHello ] "Say Hello"
        , Br [] []
        , Button [ OnClick def getTime ] "Get Time"
        , Br [] []
        , "Current Time: "
        , Translated ms
        ]
      where
        getTime _ = return $ Just $ void $
          remote askTime () $ traverse_ (putModel . MyAppState)

        sendHello _ = return $ Just $ void $
          message sayHello ()
