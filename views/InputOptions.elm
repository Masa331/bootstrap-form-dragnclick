module InputOptions exposing (view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import Models exposing (..)
import FormModel exposing (..)
import Messages exposing (..)

view : Input -> List (Html Msg)
view inp =
  let
    options =
      case inp.type' of
        Text -> [ typeEdit, placeholderEdit, labelEdit, smallUnderEdit, addon1Edit, addon2Edit ]
        Select -> [ typeEdit, labelEdit, optionsEdit, smallUnderEdit ]
        TextArea -> [ typeEdit, rowNumberEdit, placeholderEdit, labelEdit, smallUnderEdit, addon1Edit, addon2Edit ]
        Multiselect -> [ typeEdit, labelEdit, optionsEdit, smallUnderEdit ]
        FileUpload -> [ typeEdit, labelEdit, smallUnderEdit ]
        Radio -> [ typeEdit, labelEdit, optionsEdit, smallUnderEdit ]
        Checkbox -> [ typeEdit, labelEdit, smallUnderEdit ]
        Button -> [ typeEdit, labelEdit ]
        Search -> [ typeEdit, placeholderEdit, labelEdit, smallUnderEdit, addon1Edit, addon2Edit ]
        Email -> [ typeEdit, placeholderEdit, labelEdit, smallUnderEdit, addon1Edit, addon2Edit ]
        Url -> [ typeEdit, placeholderEdit, labelEdit, smallUnderEdit, addon1Edit, addon2Edit ]
        Tel -> [ typeEdit, placeholderEdit, labelEdit, smallUnderEdit, addon1Edit, addon2Edit ]
        Password -> [ typeEdit, placeholderEdit, labelEdit, smallUnderEdit, addon1Edit, addon2Edit ]
        Number -> [ typeEdit, placeholderEdit, labelEdit, smallUnderEdit, addon1Edit, addon2Edit ]
        DatetimeLocal -> [ typeEdit, placeholderEdit, labelEdit, smallUnderEdit, addon1Edit, addon2Edit ]
        Date -> [ typeEdit, placeholderEdit, labelEdit, smallUnderEdit, addon1Edit, addon2Edit ]
        Month -> [ typeEdit, placeholderEdit, labelEdit, smallUnderEdit, addon1Edit, addon2Edit ]
        Week -> [ typeEdit, placeholderEdit, labelEdit, smallUnderEdit, addon1Edit, addon2Edit ]
        Time -> [ typeEdit, placeholderEdit, labelEdit, smallUnderEdit, addon1Edit, addon2Edit ]
        Color -> [ typeEdit, labelEdit, smallUnderEdit ]
  in
    options
      |> List.map (\f -> f inp)
      |> List.concat

placeholderEdit : Input -> List (Html Msg)
placeholderEdit input =
  textEdit "Placeholder" (InputMessage << PlaceholderEdit input.id) (Maybe.withDefault "" input.placeholder)

labelEdit : Input -> List (Html Msg)
labelEdit input =
  textEdit "Label" (InputMessage << LabelEdit input.id) (Maybe.withDefault "" input.label)

smallUnderEdit : Input -> List (Html Msg)
smallUnderEdit input =
  textEdit "Small text under input" (InputMessage << SmallEdit input.id) (Maybe.withDefault "" input.small)

addon1Edit : Input -> List (Html Msg)
addon1Edit input =
  textEdit "First addon" (InputMessage << FirstAddonEdit input.id) (Maybe.withDefault "" input.addon1)

addon2Edit : Input -> List (Html Msg)
addon2Edit input =
  textEdit "Second addon" (InputMessage << SecondAddonEdit input.id) (Maybe.withDefault "" input.addon2)

typeEdit : Input -> List (Html Msg)
typeEdit input =
  selectEdit "Text input type" (InputMessage << TypeEdit input.id) stringInputTypes (inputTypeToString input.type')

textEdit : String -> (String -> Msg) -> String -> List (Html Msg)
textEdit label msg value =
  [ b [] [ text label ]
  , hr [] []
  , div [ class "form-group" ] [ Html.input [ class "form-control", onInput msg, Html.Attributes.value value ] [] ]
  ]

selectEdit : String -> (String -> Msg) -> List String -> String -> List (Html Msg)
selectEdit label msg options selected =
  let
    os =
      options
        |> List.map (\option -> Html.option [ Html.Attributes.selected (option == selected) ] [ text option ] )
  in
    [ b [] [ text label ]
    , hr [] []
    , div [ class "form-group" ] [ Html.select [ class "form-control", onInput msg ] os ]
    ]

boolEdit : String -> (Bool -> Msg) -> Bool -> List (Html Msg)
boolEdit label msg value =
  [ b [] [ text label ]
  , hr [] []
  , div [ class "form-group" ] [ Html.label [ class "form-check-label" ] [ Html.input [ type' "checkbox", class "form-check-input", onCheck msg, Html.Attributes.checked value ] [] ] ]
  ]

rowNumberEdit : Input -> List (Html Msg)
rowNumberEdit input =
  numberEdit "Number of rows" (InputMessage << RowNumberEdit input.id) input.rowNumber

numberEdit : String -> (String -> Msg) -> String -> List (Html Msg)
numberEdit label msg value =
  [ b [] [ text label ]
  , hr [] []
  , div [ class "form-group" ] [ Html.input [ type' "number", class "form-control", onInput msg, Html.Attributes.value value ] [] ]
  ]

optionsEdit : Input -> List (Html Msg)
optionsEdit input =
  let
    lifunc = (\value -> li [] [text value, a [href "javascript:void(0);", class "pull-xs-right", onClick (InputMessage (RemoveOption input.id value))] [text "remove"]] )
    lis = List.map lifunc input.options
  in
    [ b [] [text "Options"]
    , hr [] []
    , div
      [ class "input-group" ]
      [ Html.input [class "form-control", onInput (InputMessage << NewOptionEdit) ] []
      , span [class "input-group-btn"] [ Html.button [class "btn btn-secondary", type' "button", onClick (InputMessage (SaveNewOption input.id))] [text "Add"]] ]
    , div [] [ ul [] lis ]
    ]
