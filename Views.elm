module Views exposing (..)

import Html exposing (..)
import Html.Attributes exposing (class)

import Templates exposing (..)
import YourForm exposing (..)
import Markup exposing (..)

formCreator model =
  div
    []
    [ div
      [ class "row"]
      [ div
         [ class "col-sm-8"]
         [ div [class "bd-example"] (yourForm model)
         , div [class "highlight"] (markup model)]
      , div
         [ class "col-sm-4"]
         templates
      ]
    ]

inputEdit model =
  div
    []
    [ div
      [ class "row"]
      [ div
         [ class "col-sm-8"]
         [ div [class "bd-example"] (yourForm model)
         , div [class "highlight"] (markup model)]
      , div
         [ class "col-sm-4"]
         templates
      ]
    ]

templates =
  Templates.view

yourForm model =
  YourForm.view model

markup model =
  Markup.view model
