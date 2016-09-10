module Markup exposing (view)

import Html exposing (..)
import Html.Attributes exposing (..)

view =
  [ h1
    []
    [ text "Markup"
    , small [ class "text-muted" ] [ text "Copy and paste to your page" ]
    ]
  ]
