module Connection (connection,Config(..)) where

import Pure.Elm
import Pure.WebSocket as WS

import System.IO

import Shared

data Config = Config
  { socket :: WebSocket }

data Model = Model 

data Message = Startup

connection :: Config -> View
connection = run (App [Startup] [] [] (pure model) update view)
  where
    model = Model

    update Startup config model = do
      let ws = socket config
      enact ws backendImpl
      activate ws 
      pure model

    view config model = Null

backendImpl :: Endpoints _ _ _ _
backendImpl = Endpoints backendAPI msgs reqs
  where
    msgs = handleSayHello <:> WS.none
    reqs = handleAskTime <:> WS.none

handleSayHello :: MessageHandler SayHello
handleSayHello = awaiting do
  liftIO do
    putStrLn "Hello!"
    hFlush stdout

handleAskTime :: RequestHandler AskTime
handleAskTime = respondWith (const time)
