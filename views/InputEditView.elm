module InputEditView exposing (view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import Models exposing (..)
import Form exposing (..)
import Inputs exposing (..)
import Messages exposing (..)

view : Input -> List (Html Msg)
view input =
  let
    options =
      case input.type_ of
        Text -> [ typeEdit, placeholderEdit, labelEdit, smallUnderEdit, addon1Edit, addon2Edit ]
        Select -> [ typeEdit, labelEdit, smallUnderEdit, optionsEdit ]
        TextArea -> [ typeEdit, rowNumberEdit, placeholderEdit, labelEdit, smallUnderEdit, addon1Edit, addon2Edit ]
        Multiselect -> [ typeEdit, labelEdit, smallUnderEdit, optionsEdit ]
        FileUpload -> [ typeEdit, labelEdit, smallUnderEdit ]
        Radio -> [ typeEdit, labelEdit, smallUnderEdit, optionsEdit ]
        Checkbox -> [ typeEdit, labelEdit, smallUnderEdit ]
        Button -> [ typeEdit, labelEdit ]
        Password -> [ typeEdit, placeholderEdit, labelEdit, smallUnderEdit, addon1Edit, addon2Edit ]
        Number -> [ typeEdit, placeholderEdit, labelEdit, smallUnderEdit, addon1Edit, addon2Edit ]
        DatetimeLocal -> [ typeEdit, placeholderEdit, labelEdit, smallUnderEdit, addon1Edit, addon2Edit ]
        Date -> [ typeEdit, placeholderEdit, labelEdit, smallUnderEdit, addon1Edit, addon2Edit ]
        Time -> [ typeEdit, placeholderEdit, labelEdit, smallUnderEdit, addon1Edit, addon2Edit ]
        Color -> [ typeEdit, labelEdit, smallUnderEdit ]
  in
    List.map (\f -> f input) options
    |> List.concat

placeholderEdit : Input -> List (Html Msg)
placeholderEdit input =
  textEdit "Placeholder" (InputMessage << PlaceholderEdit input.id) (Maybe.withDefault "" input.placeholder)

labelEdit : Input -> List (Html Msg)
labelEdit input =
  textEdit "Label" (InputMessage << LabelEdit input.id) (Maybe.withDefault "" input.label)

smallUnderEdit : Input -> List (Html Msg)
smallUnderEdit input =
  textEdit "Description" (InputMessage << SmallEdit input.id) (Maybe.withDefault "" input.small)

addon1Edit : Input -> List (Html Msg)
addon1Edit input =
  textEdit "1 addon" (InputMessage << FirstAddonEdit input.id) (Maybe.withDefault "" input.addon1)

addon2Edit : Input -> List (Html Msg)
addon2Edit input =
  textEdit "2 addon" (InputMessage << SecondAddonEdit input.id) (Maybe.withDefault "" input.addon2)

typeEdit : Input -> List (Html Msg)
typeEdit input =
  selectEdit "Type" (InputMessage << TypeEdit input.id) stringInputTypes (inputTypeToString input.type_)

rowNumberEdit : Input -> List (Html Msg)
rowNumberEdit input =
  numberEdit "Rows" (InputMessage << RowNumberEdit input.id) input.rowNumber

textEdit : String -> (String -> Msg) -> String -> List (Html Msg)
textEdit label msg value =
  [ div
    [ class "form-group row" ]
    [ Html.label [ class "col-sm-3 col-form-label col-form-label-sm" ] [ text label ]
    , div
      [ class "col-sm-9" ]
      [ Html.input [ class "form-control form-control-sm", onInput msg, Html.Attributes.value value ] [] ]
    ]
  ]

selectEdit : String -> (String -> Msg) -> List String -> String -> List (Html Msg)
selectEdit label msg options selected =
  [ div
    [ class "form-group row" ]
    [ Html.label [ class "col-sm-3 col-form-label col-form-label-sm" ] [ text label ]
    , div
      [ class "col-sm-9" ]
      [ Html.select
        [ class "form-control form-control-sm", onInput msg ]
        (List.map (\o -> Html.option [ Html.Attributes.selected (o == selected) ] [ text o ] ) options)
      ]
    ]
  ]

numberEdit : String -> (String -> Msg) -> String -> List (Html Msg)
numberEdit label msg value =
  [ div
    [ class "form-group row" ]
    [ Html.label [ class "col-sm-3 col-form-label col-form-label-sm" ] [ text label ]
    , div
      [ class "col-sm-9" ]
      [ Html.input [ type_ "number", class "form-control form-control-sm", onInput msg, Html.Attributes.value value ] [] ]
    ]
  ]

optionsEdit : Input -> List (Html Msg)
optionsEdit input =
  let
    lifunc =
      (\value -> li
                   []
                   [text (value ++ " ")
                   , small
                     []
                     [a [href "javascript:void(0);", onClick (InputMessage (RemoveOption input.id value))] [text "remove"]]] )
  in
    [ div
      [ class "form-group row" ]
      [ Html.label [ class "col-sm-3 col-form-label col-form-label-sm" ] [ text "Options" ]
      , div
        [ class "col-sm-9" ]
        [ div
          [ class "input-group" ]
          [ Html.input
          -- I need to set the value here so everytime user submits new option the input gets cleared however
          --    i would need to have model here.. https://github.com/elm-lang/html/issues/2
            [class "form-control form-control-sm", onInput (NewOptionEdit) ]
            []
          , span
            [ class "input-group-btn" ]
            [ Html.button
              [class "btn btn-sm btn-secondary"
              , type_ "button"
              , onClick (InputMessage (SaveNewOption input.id))] [text "Add"]
              ]
          ]
        , ul [ class "list-unstyled" ] (List.map lifunc input.options)
        ]
      ]
    ]
