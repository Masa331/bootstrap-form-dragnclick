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
      List.concat [ placeholderEdit (extractPlaceholder inp), labelEdit (extractLabel inp) ]
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
    , div  [ class "form-group" ] [ input [class "form-control", onInput (InputMessage << PlaceholderEdit), value placeholderText] [ ] ]
    ]

labelEdit : Maybe String -> List (Html Msg)
labelEdit string =
  let
    labelText =
      case string of
        Just s -> s
        Nothing -> "ehe"
  in
    [ b [] [ text "Label" ]
    , hr [] []
    , div [ class "form-group" ] [ input [ class "form-control", onInput (InputMessage << LabelEdit), value labelText ] [] ]
    ]
