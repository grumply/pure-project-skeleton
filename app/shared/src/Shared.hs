{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module Shared where

import Pure
import Pure.WebSocket as WS

mkMessage "SayHello" [t|Txt|]

mkRequest "AskTime" [t|() -> Time|]

backendAPI :: API '[SayHello] '[AskTime]
backendAPI = api msgs reqs
  where
    msgs = sayHello <:> WS.none
    reqs = askTime <:> WS.none
