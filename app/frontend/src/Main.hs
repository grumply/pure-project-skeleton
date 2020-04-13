module Main where

import Pure
import Pure.WebSocket

import Shared

main :: IO ()
main = do
  ws <- clientWS "127.0.0.1" 8081
  inject body (app ws)

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

helloButton :: WebSocket -> View
helloButton ws =
  let
    say = notify backendAPI ws sayHello ()
  in
    Button <| OnClick (const say) |>
      [ "Say Hello" ]

timeButton :: WebSocket -> (Time -> IO ()) -> View
timeButton ws setTime =
  let
    set = remote backendAPI ws askTime () setTime
  in
    Button <| OnClick (const set) |>
      [ "Get Time" ]

timeDisplay :: Maybe Time -> View
timeDisplay Nothing  = Null
timeDisplay (Just t) =
  Div <||>
    [ "Latest time: "
    , toZonedDateTime t
    ]