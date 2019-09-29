{-# LANGUAGE OverloadedStrings #-}
module Main where

import Pure
import Pure.WebSocket

import Shared

main :: IO ()
main = clientWS "127.0.0.1" 8081 >>= inject body . app

app :: WebSocket -> View
app = Component $ \self ->
  let
    setTime t = modify_ self $ \_ _ -> Just t
  in
    def
      { construct = return Nothing
      , render = \ws mt ->
        Div <||>
          [ helloButton ws
          , timeButton ws setTime
          , timeDisplay mt
          ]
      }

helloButton ws =
  let
    send = notify backendAPI ws sayHello ()
  in
    Button <| OnClick (const send) |>
      [ "Say Hello" ]

timeButton ws setTime =
  let
    send = remote backendAPI ws askTime () setTime
  in
    Button <| OnClick (const send) |>
      [ "Get Time" ]

timeDisplay Nothing  = Null
timeDisplay (Just t) =
  Div <||>
    [ "Latest time: "
    , toZonedDateTime t
    ]