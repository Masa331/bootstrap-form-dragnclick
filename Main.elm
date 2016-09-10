module Main exposing (..)

import Html exposing (div)
import Html.App
import Html.Attributes exposing (class)

import Views
import Models
import Messages

init = (Models.initialModel, Cmd.none)

view model =
  div
    []
    [div
      [class "row"]
      [ div
         [class "col-sm-4"]
         Views.templatesView
      , div
         [class "col-sm-8"]
         (Views.yourForm model)
      ]
    ,div
      [class "row"]
      [div
        [class "col-sm-12"]
        Views.markup
      ]
    ]

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
