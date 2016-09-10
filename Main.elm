port module Main exposing (..)

import Html
import Html.App
import Html.Attributes

initialModel = ""

init = (initialModel, Cmd.none)

view model =
  Html.text "Ahoj svete!"

update msg model =
  (model, Cmd.none)

subscriptions model =
  Sub.none

main =
  Html.App.program
  { init = init
  , view = view
  , update = update
  , subscriptions = subscriptions
  }
