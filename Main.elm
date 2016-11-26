module Main exposing (..)

import Html exposing (div)
import Mouse

import Views
import Models
import Messages
import Updates
import Utils
import Navigation

init location =
  (Models.initial, Cmd.none)

view model =
  Views.view model

update msg model =
  Updates.update msg model

subscriptions model =
  if List.any .dragged model.inputs then
    Sub.batch [ Mouse.moves (Messages.MouseMessage << Messages.MouseMove)
              , Mouse.ups (Messages.MouseMessage << Messages.MouseUp)
              ]
  else
    Utils.determinedFormMap Messages.MapDetermined

main =
  Navigation.program Messages.UrlChange
  { init = init
  , view = view
  , update = update
  , subscriptions = subscriptions
  }
