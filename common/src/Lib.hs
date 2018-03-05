{-# LANGUAGE OverloadedStrings, TemplateHaskell, DeriveAnyClass, NoMonomorphismRestriction #-}
module Lib where

import Pure.Data
import Pure.Service
import Pure.WebSocket

mkMessage "SayHello" [t|()|]

mkRequest "AskTime" [t|() -> Millis|]

backendAPI = api msgs reqs
  where
    msgs =
          sayHello <:>
          none
    reqs =
          askTime <:>
          none

backendService = Service {..}
  where
    key = "server"

    build base = do
      ws <- createWebSocket ?serverIp ?serverPort
      return (ws *:* base)

    prime = void $ do
      connectWebSocket ?serverIp ?serverPort

message = apiMessageWith backendAPI backendService

remote prxy req onSuccess = do
  pr <- withPromise $ \pr -> void $ do
    u <- fresh
    apiRequestWith backendAPI backendService prxy (u,req) $ \done rsp -> lift $ do
      either (const $ fulfill pr Nothing) (fulfill pr . Just) rsp
      done
  attach pr def { success = onSuccess }

