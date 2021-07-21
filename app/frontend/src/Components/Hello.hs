module Components.Hello (Hello(..)) where

import Pure.Elm.Component hiding (name)
import Pure.WebSocket as WS

import Shared

data Hello = Hello
  { socket :: WebSocket }

instance Component Hello where
  data Model Hello = Model
    { name :: Txt }

  model = Model def

  data Msg Hello 
    = SetName Txt
    | SendHello

  upon = \case
    SetName name -> setName name
    SendHello    -> sendHello

  view _ Model { name } =
    Div <| Themed @Hello |>
      [ Input <| OnInput onInput . Value name
      , Button <| OnClick onClick |> 
        [ "Send Hello" ]
      ]

onInput :: Handler Hello
onInput = withInput (command . SetName)

onClick :: Handler Hello
onClick _ = command SendHello

setName :: Txt -> Update Hello
setName nm _ mdl = 
  pure mdl { name = nm }

sendHello :: Update Hello
sendHello Hello { socket } mdl@Model { name } = do
  message backendAPI socket sayHello name
  pure mdl

instance Theme Hello where
  theme c =
    is c do
      font-size =: 3em