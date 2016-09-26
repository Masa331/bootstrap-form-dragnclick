module InputOptions exposing (view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import String exposing (..)

import Models exposing (..)
import Messages exposing (..)

view : Input -> List (Html Msg)
view inp =
  case inp of
    TextInput attrs ->
      List.concat [ placeholderEdit (Debug.log "neco" (extractPlaceholder inp)), labelEdit inp ]
    _ ->
      [ b [] [text "Not yet implemented ;)"]
      ]



placeholderEdit : Maybe String -> List (Html Msg)
placeholderEdit string =
  let
    placeholderText =
      case string of
        Just s -> s
        Nothing -> "ehe"
  in
    [ b [] [text "Placeholder"]
    , hr [] []
    , div [] [ input [class "form-control", onInput (InputMessage << PlaceholderEdit), value placeholderText] [ ] ]
    ]

labelEdit : Input -> List (Html Msg)
labelEdit inp =
  [ b [] [ text "Label" ]
  , hr [] []
  , div [] [ input [ class "form-control", onInput (InputMessage << LabelEdit) ] [] ]
  ]
