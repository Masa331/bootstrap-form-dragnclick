module YourForm exposing (view)

import Html exposing (..)
import Html.Attributes exposing (..)

view model =
  [ heading
  , form model
  ]

-------------
-- Private --
-------------

heading =
  h1
  []
  [ text "Your Form"
  , small [ class "text-muted" ] [ text "Drag to move, Right click to edit" ]
  ]

form model =
  text "form"
