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
  case msg of
    Messages.InputMessage inputMsg ->
      Updates.inputUpdate inputMsg model
    Messages.FormMessage formMsg ->
      Updates.formUpdate formMsg model
    Messages.MouseMessage mouseMsg ->
      Updates.mouseUpdate mouseMsg model
    Messages.MapDetermined map ->
      (updateInputsDimensions model map, Cmd.none)
    Messages.UrlChange location ->
      ({ model | history = location :: model.history }
      , Cmd.none)

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

-------------
-- Private --
-------------

updateInputsDimensions model map =
  let
    flatMap = List.concat map
    updateFunction = (\input -> { input | dimensions = (List.filter (\e -> e.id == toString input.id) flatMap) |> List.head })

    inputsWithUpdatedDimesnions =
      List.map updateFunction model.inputs
  in
    { model | inputs = inputsWithUpdatedDimesnions }
