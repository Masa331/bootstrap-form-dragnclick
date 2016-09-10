module TemplatesView exposing (view)

import Html exposing (..)
import Html.Attributes exposing (..)

view =
  [ h1
    []
    [ text "Form elements"
    , small [ class "text-muted" ] [ text "Grab n drag to your form" ]
    ]
  ]
