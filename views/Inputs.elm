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
    input = Html.input (inputAttributes inp) []
    links = []
    id = extractId inp
  in
    formGroup [ label inp, input ] id

textAreaHtml : Input -> Html Msg
textAreaHtml inp =
  let
    area = textarea (inputAttributes inp) []
    id = extractId inp
  in
    formGroup [ label inp, area ] id

selectHtml : Input -> Html Msg
selectHtml inp =
  let
    s1 = option [] [text "1"]
    s2 = option [] [text "2"]
    s3 = option [] [text "3"]
    select = Html.select [] [s1, s2, s3]
    id = extractId inp
  in
    formGroup [ label inp, select ] id

-------------
-- Helpers --
-------------

editAndRemoveLink id =
  div [class "edit-and-remove-link"] [editLink id, removeLink id]

editLink id =
  a [href "javascript:void(0);", onClick (FormMessage (EditInput id))] [text "Edit"]

removeLink id =
  a [href "javascript:void(0);", onClick (FormMessage (RemoveInput id))] [text "Remove"]

formGroup : List (Html Msg) -> Id -> Html Msg
formGroup els id =
  let
    links = [editAndRemoveLink id]
    elementsAndLinks = List.append els links
  in
    div [ class "form-group" ] elementsAndLinks

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
    Html.label [ for labelText ] [ text forValue ]

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
-----------


multiselectHtml : Input -> Html Msg
multiselectHtml inp =
  text "ho"

fileUploadHtml : Input -> Html Msg
fileUploadHtml inp =
  text "ho"

radioHtml : Input -> Html Msg
radioHtml inp =
  text "ho"

checkboxHtml : Input -> Html Msg
checkboxHtml inp =
  text "ho"

buttonHtml : Input -> Html Msg
buttonHtml inp =
  text "ho"
