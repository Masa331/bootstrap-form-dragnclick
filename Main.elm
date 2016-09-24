module Main exposing (..)

import Html exposing (div)
import Html.App

import Views
import Models
import Messages
import Updates

init = (Models.new, Cmd.none)

view model =
  Views.view model

update msg model =
  (model, Cmd.none)
  -- case msg of
  --   Messages.InputMessage inputMsg ->
  --     Updates.inputUpdate inputMsg model
  --   Messages.FormMessage formMsg ->
  --     Updates.formUpdate formMsg model

subscriptions model =
  Sub.none

main =
  Html.App.program
  { init = init
  , view = view
  , update = update
  , subscriptions = subscriptions
  }
