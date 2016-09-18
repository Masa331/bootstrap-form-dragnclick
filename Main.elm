module Main exposing (..)

import Html exposing (div)
import Html.App
import Html.Attributes exposing (class)

import Views
import Models
import Messages
import Updates

init = (Models.initialModel, Cmd.none)

view model =
  div
    []
    [ div
      [ class "row"]
      [ div
         [ class "col-sm-8"]
         [ div [class "bd-example"] (Views.yourForm model)
         , div [class "highlight"] (Views.markup model)]
      , div
         [ class "col-sm-4"]
         Views.templates
      ]
    ]

update msg model =
  case msg of
    Messages.InputMessage inputMsg ->
      Updates.inputUpdate inputMsg model
    Messages.FormMessage formMsg ->
      Updates.formUpdate formMsg model

subscriptions model =
  Sub.none

main =
  Html.App.program
  { init = init
  , view = view
  , update = update
  , subscriptions = subscriptions
  }
