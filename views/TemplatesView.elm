module TemplatesView exposing (view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import Messages exposing (..)

view =
  List.append [heading] [textInput, submit]

-------------
-- Private --
-------------

heading =
  h1
    []
    [ text "Form elements"
    , small [ class "text-muted" ] [ text "Grab n drag to your form" ]
    ]

textInput =
  div
    [class "form-group"]
    [ label [] [text "Textove pole"]
    , input [class "form-control"] []
    , a [href "#", onClick AddTextInput] [text "add"]
    , hr [] []
    ]

submit =
  div
    [class "form-group"]
    [ button [class "btn btn-primary"] [text "Submit button"]
    , hr [] []
    ]
