{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module Main where

import Pure.Server
import Pure.Connection
import Pure.WebSocket

import Lib

main :: IO ()
main = run (app "127.0.0.1" 8000)

app ip port = Server {..}
  where
    key   = "server"
    build = return
    prime = return ()
    connection = conn

conn _ = Connection {..}
  where
    build = return
    prime = void $ do
      enact backendImpl
      setExhaustLimit maxBound

backendImpl = Impl backendAPI msgs reqs
  where
    msgs =
          handleSayHello <:>
          none
    reqs =
          handleAskTime <:>
          none

-- Note that if this server is running via `npm start`, you will not see "Got Hello!"
handleSayHello = accepts sayHello $ \_done q ->
  lift_ $ for q $ \_req ->
    liftIO $ putStrLn "Got Hello!"

handleAskTime = responds askTime $ \_done q ->
  lift_ $ for q $ \(rsp,(_rqId,_req)) -> do
    now <- millis
    rsp (Right now)
