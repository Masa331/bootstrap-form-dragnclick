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
      List.concat [ placeholderEdit (extractPlaceholder inp), labelEdit (extractLabel inp), smallUnderEdit (extractSmall inp), typeEdit, addon1Edit, addon2Edit, sizeEdit, disabledEdit (extractDisabled inp), readonlyEdit ]
    _ ->
      [ b [] [text "Not yet implemented ;)"]
      ]



placeholderEdit : Maybe String -> List (Html Msg)
placeholderEdit string =
  let
    placeholderText =
      case string of
        Just s -> s
        Nothing -> ""
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
        Nothing -> ""
  in
    [ b [] [ text "Label" ]
    , hr [] []
    , div [ class "form-group" ] [ input [ class "form-control", onInput (InputMessage << LabelEdit), value labelText ] [] ]
    ]

-- smallUnderEdit : List (Html Msg)
smallUnderEdit string =
  let
    smallText =
      case string of
        Just s -> s
        Nothing -> ""
  in
    [ b [] [ text "Small text under input" ]
    , hr [] []
    , div [ class "form-group" ] [ input [ class "form-control", value smallText, onInput (InputMessage << SmallEdit) ] [ ] ]
    ]

typeEdit : List (Html Msg)
typeEdit =
  let
    o1 = option [] [ text "text" ]
    o2 = option [] [ text "search" ]
    o3 = option [] [ text "email" ]
    o4 = option [] [ text "url" ]
    o5 = option [] [ text "tel" ]
    o6 = option [] [ text "password" ]
    o7 = option [] [ text "number" ]
    o8 = option [] [ text "datetime-local" ]
    o9 = option [] [ text "date" ]
    o10 = option [] [ text "month" ]
    o11 = option [] [ text "week" ]
    o12 = option [] [ text "time" ]
    o13 = option [] [ text "color" ]
    types = [o1, o2, o3, o4, o5, o6, o7, o8, o9, o10, o11, o12, o13]
  in
    [ b [] [ text "Text input type" ]
    , hr [] []
    , div [ class "form-group" ] [ Html.select [ class "form-control" ] types ]
    ]

addon1Edit : List (Html Msg)
addon1Edit =
  [ b [] [ text "First addon" ]
  , hr [] []
  , div [ class "form-group" ] [ input [ class "form-control" ] [] ]
  ]

addon2Edit : List (Html Msg)
addon2Edit =
  [ b [] [ text "Second addon" ]
  , hr [] []
  , div [ class "form-group" ] [ input [ class "form-control" ] [] ]
  ]

sizeEdit : List (Html Msg)
sizeEdit =
  let
    s1 = option [] [ text "small" ]
    s2 = option [] [ text "normal" ]
    s3 = option [] [ text "large" ]
    types = [s1, s2, s3]
  in
    [ b [] [ text "Size edit" ]
    , hr [] []
    , div [ class "form-group" ] [ Html.select [ class "form-control" ] types ]
    ]

-- disabledEdit : List (Html Msg)
disabledEdit value =
  [ b [] [ text "Disabled" ]
  , hr [] []
  , div [ class "form-group" ] [ label [ class "form-check-label" ] [ input [ type' "checkbox", class "form-check-input", onCheck (InputMessage << DisabledEdit), Html.Attributes.checked value ] [] ] ]
  ]

readonlyEdit : List (Html Msg)
readonlyEdit =
  [ b [] [ text "Readonly" ]
  , hr [] []
  , div [ class "form-group" ] [ label [ class "form-check-label" ] [ input [ type' "checkbox", class "form-check-input" ] [] ] ]
  ]
