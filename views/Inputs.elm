module Inputs exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import String exposing (..)

import Models exposing (..)
import Messages exposing (..)

inputHtml : Input -> Html Msg
inputHtml input =
  case input of
    TextInput _ ->
      textInputHtml input
    TextArea _ ->
      textAreaHtml input
    Select _ ->
      selectHtml input
    Multiselect _ ->
      multiselectHtml input
    FileUpload _ ->
      fileUploadHtml input
    Radio _ ->
      radioHtml input
    Checkbox _ ->
      checkboxHtml input
    Button _ ->
      buttonHtml input

textInputHtml : Input -> Html Msg
textInputHtml inp =
  let
    inputLabel = label inp
    input = Html.input (inputAttributes inp) []
  in
    div [ class "form-group" ] [ inputLabel, input, editAndRemoveLink inp ]

textAreaHtml : Input -> Html Msg
textAreaHtml inp =
  let
    area = textarea (inputAttributes inp) []
  in
    div [ class "form-group" ] [ label inp, area, editAndRemoveLink inp ]

selectHtml : Input -> Html Msg
selectHtml inp =
  let
    s1 = option [] [text "1"]
    s2 = option [] [text "2"]
    s3 = option [] [text "3"]
    select = Html.select [class "form-control"] [s1, s2, s3]
  in
    div [ class "form-group" ] [ label inp, select, editAndRemoveLink inp ]

multiselectHtml : Input -> Html Msg
multiselectHtml inp =
  let
    s1 = option [] [text "1"]
    s2 = option [] [text "2"]
    s3 = option [] [text "3"]
    select = Html.select [class "form-control", multiple True] [s1, s2, s3]
  in
    div [ class "form-group" ] [ label inp, select, editAndRemoveLink inp ]

fileUploadHtml : Input -> Html Msg
fileUploadHtml inp =
  let
    input = Html.input [type' "file"] []
  in
    div [ class "form-group" ] [ label inp, input, editAndRemoveLink inp ]

radioHtml : Input -> Html Msg
radioHtml inp =
  let
    leg = legend [] [ text "Radios" ]
    r1 = div [] [Html.label [] [input [type' "radio", name "radioOption", Html.Attributes.id "radioOption1", value "option1"] [], text "Option 1"]]
    r2 = div [] [Html.label [] [input [type' "radio", name "radioOption", Html.Attributes.id "radioOption2", value "option2"] [], text "Option 2"]]
  in
    fieldset [ class "form-group" ] [ leg, r1, r2, editAndRemoveLink inp ]

checkboxHtml : Input -> Html Msg
checkboxHtml inp =
  let
    input = Html.input [type' "checkbox"] []
    label = Html.label [] [input, text "Check me out"]
  in
    div [ class "form-check" ] [ label, editAndRemoveLink inp ]

buttonHtml : Input -> Html Msg
buttonHtml inp =
  div
    [ class "my-container" ]
    [ button [ type' "submit" ] [ text "Submit" ]
    , editAndRemoveLink inp ]

-------------
-- Helpers --
-------------

editAndRemoveLink inp =
  div
    [class "edit-and-remove-link"]
    -- [ p [] [editLink inp], p [] [removeLink inp]]
    [ editLink inp, text " | ", removeLink inp, text " | ", moveUpLink inp, text " | ", moveDownLink inp]

editLink id =
  a [href "javascript:void(0);", onClick (FormMessage (EditInput (extractId id)))] [text "Edit"]

removeLink id =
  a [href "javascript:void(0);", onClick (FormMessage (RemoveInput (extractId id)))] [text "Remove"]

moveUpLink id =
  a [href "javascript:void(0);"] [text "Move up"]

moveDownLink id =
  a [href "javascript:void(0);"] [text "Move Down"]

label : Input -> Html Msg
label inp =
  let
    (labelText, forValue) =
      case inp of
        TextInput attrs -> (Maybe.withDefault "default" attrs.label, "input" ++ toString attrs.id)
        TextArea attrs -> (Maybe.withDefault "default" attrs.label, "input" ++ toString attrs.id)
        Select attrs -> (Maybe.withDefault "default" attrs.label, "input" ++ toString attrs.id)
        Multiselect attrs -> (Maybe.withDefault "default" attrs.label, "input" ++ toString attrs.id)
        FileUpload attrs -> (Maybe.withDefault "default" attrs.label, "input" ++ toString attrs.id)
        Radio attrs -> (Maybe.withDefault "default" attrs.label, "input" ++ toString attrs.id)
        Checkbox attrs -> (Maybe.withDefault "default" attrs.label, "input" ++ toString attrs.id)
        Button attrs -> (Maybe.withDefault "default" attrs.label, "input" ++ toString attrs.id)
  in
    Html.label [ for forValue ] [ text labelText ]

inputAttributes : Input -> List (Html.Attribute a)
inputAttributes inp =
  case inp of
    TextInput attrs ->
      textInputAttrs attrs
      |> List.filterMap identity
    TextArea attrs ->
      textAreaAttrs attrs
      |> List.filterMap identity
    Select attrs ->
      selectAttrs attrs
      |> List.filterMap identity
    Multiselect attrs ->
      multiselectAttrs attrs
      |> List.filterMap identity
    FileUpload attrs ->
      fileUploadAttrs attrs
      |> List.filterMap identity
    Radio attrs ->
      radioAttrs attrs
      |> List.filterMap identity
    Checkbox attrs ->
      checkboxAttrs attrs
      |> List.filterMap identity
    Button attrs ->
      buttonAttrs attrs
      |> List.filterMap identity

-- textInputAttrs : Id -> ClassList -> Placeholder -> List (Html.Attribute a)
textInputAttrs attrs =
  [idToAttr attrs.id, classesToAttr attrs.classList, placeholderToAttr attrs.placeholder, Just (type' "text")]

-- textAreaAttrs : Id -> ClassList -> Placeholder -> RowNumber -> List (Html.Attribute a)
textAreaAttrs attrs =
  [idToAttr attrs.id, classesToAttr attrs.classList, placeholderToAttr attrs.placeholder, Just (rows attrs.rowNumber)]

selectAttrs attrs =
  [idToAttr attrs.id, classesToAttr attrs.classList, Just (type' "text")]

multiselectAttrs attrs =
  [idToAttr attrs.id, classesToAttr attrs.classList, Just (type' "text")]

fileUploadAttrs attrs =
  [idToAttr attrs.id, classesToAttr attrs.classList, Just (type' "text")]

radioAttrs attrs =
  [idToAttr attrs.id, classesToAttr attrs.classList, Just (type' "text")]

checkboxAttrs attrs =
  [idToAttr attrs.id, classesToAttr attrs.classList, Just (type' "text")]

buttonAttrs attrs =
  [idToAttr attrs.id, classesToAttr attrs.classList, Just (type' "text")]

-------
idToAttr : Id -> Maybe (Html.Attribute a)
idToAttr id =
  Just (Html.Attributes.id ("input" ++ toString id))

-- classesToAttribute : Id -> Maybe (Html.Attribute a)
classesToAttr classList =
  Just (class (String.join " " classList))

placeholderToAttr : Placeholder -> Maybe (Html.Attribute a)
placeholderToAttr plac =
  Maybe.map placeholder plac
