module Components.Home (Home(..)) where

import Pure.Elm.Application (lref)
import Pure.Elm.Component

data Home = Home

instance Component Home where
  view _ _ =
    Div <| Themed @Home |>
      [ A <| lref "/time"  |> [ "Server Time App" ]
      , A <| lref "/hello" |> [ "Hello App" ]
      ]

instance Theme Home where
  theme c =
    is c do
      child (tag A) do
        display =: block