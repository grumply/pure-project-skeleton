{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module Shared where

import Pure
import Pure.WebSocket as WS

mkMessage "SayHello" [t|()|]

mkRequest "AskTime" [t|() -> Time|]

backendAPI = api msgs reqs
  where
    msgs =
          sayHello <:>
          WS.none
    reqs =
          askTime <:>
          WS.none
