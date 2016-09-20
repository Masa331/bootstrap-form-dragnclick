module InputOptions exposing (view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import String exposing (..)

import Models exposing (..)
import Messages exposing (..)

view model =
  [ b [] [text "Placeholder"]
  , hr [] []
  , div [] [ input [class "form-control", onInput (InputMessage << PlaceholderEdit)] [] ]

  , b [] [text "Placeholder"]
  , hr [] []
  , div [] [ input [class "form-control"] [] ]
  ]

-------------
-- Private --
-------------
