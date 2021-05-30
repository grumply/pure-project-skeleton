module App (app,Config(..)) where

import Pure.Elm hiding (time)
import Pure.WebSocket as WS

import Shared

data Config = Config
  { socket :: WebSocket
  }

data Model = Model
  { serverTime :: Maybe Time
  }

data Message 
  = SetServerTime Time

app :: Config -> View
app = run (App [] [] [] (pure model) update view)
  where
    model = Model Nothing

    update (SetServerTime t) _ m = 
      pure m { serverTime = Just t }

    view config model = 
      let
        msg = message backendAPI (socket config)
        req = request backendAPI (socket config)
      in 
        Div <||>
          [ let 
              hello = msg sayHello ()
            in 
              messageExample hello

          , let 
              getServerTime = req askTime () (command . SetServerTime)
            in 
              requestExample model getServerTime
          ]

messageExample :: IO () -> View
messageExample hello =
  Button <| OnClick (const hello) |> [ "Say Hello" ]

requestExample :: Model -> IO () -> View
requestExample model getServerTime = 
  Div <||>
    [ Button <| OnClick (const getServerTime) |> [ "Get Time" ]
    , maybe Null zonedTime (serverTime model) 
    ]
  where
    zonedTime t = 
      Div <||>
        [ "Server time: "
        , toZonedDateTime t
        ]
