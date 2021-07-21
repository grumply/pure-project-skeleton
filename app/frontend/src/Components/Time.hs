module Components.Time (Time(..)) where

import qualified Pure.Elm.Component as Time (Time)
import Pure.Elm.Component hiding (Time)
import Pure.WebSocket

import Shared

data Time = Time
  { socket :: WebSocket
  }

instance Component Time where
  data Model Time = Model
    { serverTime :: Maybe Time.Time 
    }

  model = Model Nothing

  data Msg Time 
    = GetServerTime
    | SetServerTime Time.Time

  startup = [GetServerTime]

  upon = \case
    GetServerTime   -> getServerTime
    SetServerTime t -> setServerTime t

  view _ Model { serverTime } = 
    Div <| Themed @Time |>
      [ Button <| OnClick onClick |> [ "Request Server Time" ]
      , Div <||>
        [ "Server time: "
        , maybe Null toZonedDateTime serverTime
        ]
      ]

onClick :: Handler Time
onClick _ = command GetServerTime

getServerTime :: Update Time
getServerTime Time { socket } mdl = do
  request backendAPI socket askTime () (command . SetServerTime)
  pure mdl

setServerTime :: Time.Time -> Update Time
setServerTime t _ mdl = pure mdl { serverTime = Just t }

instance Theme Time where
  theme c =
    is c do
      pure ()