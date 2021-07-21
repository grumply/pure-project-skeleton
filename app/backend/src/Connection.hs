module Connection (Connection(..)) where

import Pure.Elm.Component hiding (Start,start)
import Pure.WebSocket as WS

import System.IO

import Shared

data Connection = Connection
  { socket :: WebSocket }

instance Component Connection where
  data Msg Connection = Start

  startup = [Start]

  upon = \case
    Start -> start

start :: Update Connection
start Connection { socket } mdl = do
  enact socket backendImpl
  activate socket 
  pure mdl

backendImpl :: Endpoints _ _ _ _
backendImpl = Endpoints backendAPI msgs reqs
  where
    msgs = handleSayHello <:> WS.none
    reqs = handleAskTime <:> WS.none

handleSayHello :: MessageHandler SayHello
handleSayHello = awaiting do
  nm <- acquire
  liftIO do
    putStrLn ("Hello from: " <> fromTxt nm)
    hFlush stdout

handleAskTime :: RequestHandler AskTime
handleAskTime = respondWith (const time)
