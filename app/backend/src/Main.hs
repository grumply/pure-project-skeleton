module Main where

import Pure
import Pure.Server
import Pure.WebSocket as WS

import Shared

import Control.Monad
import System.IO

main :: IO ()
main = inject body (server ()) >> hSetBuffering stdout NoBuffering >> sleep
  where
    sleep = forever (delay (Seconds 60 0))

server :: () -> View
server = Component $ \_self -> def
    { construct = return ()
    , render    = \_ _ -> Server "127.0.0.1" 8081 conn
    }

conn :: WebSocket -> View
conn = Component $ \self -> def
  { construct = return ()
  , executing = \st -> do
      ws <- ask self
      enact ws backendImpl
      activate ws
      pure st
  }

backendImpl :: ( msgs ~ '[SayHello]
               , reqs ~ '[AskTime]
               ) => Implementation msgs reqs msgs reqs
backendImpl = Impl backendAPI msgs reqs
  where
    msgs = handleSayHello <:> WS.none
    reqs = handleAskTime <:> WS.none

handleSayHello :: MessageHandler SayHello
handleSayHello = awaiting $ liftIO (putStrLn "Hello!")

handleAskTime :: RequestHandler AskTime
handleAskTime = responding (liftIO time >>= reply)