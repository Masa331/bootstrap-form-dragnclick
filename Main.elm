module Main exposing (..)

import Html exposing (div)
import Html.App
import Mouse

import Views
import Models
import Messages
import Updates
import Utils

init = (Models.initial, Cmd.none)

view model =
  Views.view model

update msg model =
  case msg of
    Messages.InputMessage inputMsg ->
      Updates.inputUpdate inputMsg model
    Messages.FormMessage formMsg ->
      Updates.formUpdate formMsg model
    Messages.MouseMessage mouseMsg ->
      Updates.mouseUpdate mouseMsg model
    Messages.MapDetermined map ->
      ({ model | elementMap = map }, Cmd.none)


subscriptions model =
  if List.any .dragged model.form then
    Sub.batch [ Mouse.moves (Messages.MouseMessage << Messages.MouseMove)
              , Mouse.ups (Messages.MouseMessage << Messages.MouseUp)
              ]
  else
    Utils.determinedFormMap Messages.MapDetermined

main =
  Html.App.program
  { init = init
  , view = view
  , update = update
  , subscriptions = subscriptions
  }
