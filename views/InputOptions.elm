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
      List.concat [ placeholderEdit (extractPlaceholder inp), labelEdit (extractLabel inp), smallUnderEdit (extractSmall inp), typeEdit (extractType inp), addon1Edit (extractAddon1 inp), addon2Edit (extractAddon2 inp), sizeEdit (extractSize inp), disabledEdit (extractDisabled inp), readonlyEdit (extractReadonly inp) ]
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

typeEdit : InputType -> List (Html Msg)
typeEdit type' =
  let
    actualType = typeToText type'
    o = (\optionType -> option [ selected (if optionType == actualType then True else False) ] [ text optionType ] )
    options = [o "text", o "search", o "email", o "url", o "tel", o "password", o "number", o "datetime-local", o "date", o "month", o "week", o "time", o "color"]
  in
    [ b [] [ text "Text input type" ]
    , hr [] []
    , div [ class "form-group" ] [ Html.select [ class "form-control", onInput (InputMessage << TypeEdit) ] options ]
    ]

addon1Edit : Maybe String -> List (Html Msg)
addon1Edit string =
  let
    addonText =
      case string of
        Just s -> s
        Nothing -> ""
  in
  [ b [] [ text "First addon" ]
  , hr [] []
  , div [ class "form-group" ] [ input [ class "form-control", value addonText, onInput (InputMessage << FirstAddonEdit) ] [] ]
  ]

addon2Edit : Maybe String -> List (Html Msg)
addon2Edit string =
  let
    addonText =
      case string of
        Just s -> s
        Nothing -> ""
  in
  [ b [] [ text "Second addon" ]
  , hr [] []
  , div [ class "form-group" ] [ input [ class "form-control", value addonText, onInput (InputMessage << SecondAddonEdit) ] [] ]
  ]

sizeEdit : Size -> List (Html Msg)
sizeEdit size =
  let
    options =
      case size of
        Small -> [ option [selected True] [ text "small" ], option [] [ text "normal" ], option [] [ text "large" ] ]
        Normal -> [ option [] [ text "small" ], option [selected True] [ text "normal" ], option [] [ text "large" ] ]
        Large -> [ option [] [ text "small" ], option [] [ text "normal" ], option [selected True] [ text "large" ] ]
  in
    [ b [] [ text "Size edit" ]
    , hr [] []
    , div [ class "form-group" ] [ Html.select [ class "form-control", onInput (InputMessage << SizeEdit) ] options ]
    ]

-- disabledEdit : List (Html Msg)
disabledEdit value =
  [ b [] [ text "Disabled" ]
  , hr [] []
  , div [ class "form-group" ] [ label [ class "form-check-label" ] [ input [ type' "checkbox", class "form-check-input", onCheck (InputMessage << DisabledEdit), Html.Attributes.checked value ] [] ] ]
  ]

-- readonlyEdit : List (Html Msg)
readonlyEdit value =
  [ b [] [ text "Readonly" ]
  , hr [] []
  , div [ class "form-group" ] [ label [ class "form-check-label" ] [ input [ type' "checkbox", class "form-check-input", onCheck (InputMessage << ReadonlyEdit), Html.Attributes.checked value ] [] ] ]
  ]
