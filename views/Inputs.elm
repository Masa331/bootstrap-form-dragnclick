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
    TextInput (a, b, c, _) ->
      textInputHtml input
    TextArea (a, b, c, d, _) ->
      textAreaHtml input
    Select (a) ->
      selectHtml input
    Multiselect (a) ->
      multiselectHtml input
    FileUpload (a) ->
      fileUploadHtml input
    Radio (a) ->
      radioHtml input
    Checkbox (a) ->
      checkboxHtml input
    Button (a) ->
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
    -- formGroup [ label inp, area ]
    formGroup [ label inp, area ] id

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
    (txt, forx) =
      case inp of
        TextInput (a, _, _, d) -> (Maybe.withDefault "default" d, "input" ++ toString a)
        TextArea (a, _, _, _, e) -> (Maybe.withDefault "default" e, "input" ++ toString a)
        _ -> ("ahoj", "input2")
  in
    Html.label [ for forx ] [ text txt ]

inputAttributes : Input -> List (Html.Attribute a)
inputAttributes inp =
  case inp of
    TextInput (a, b, c, d) ->
      textInputAttributes a b c
    TextArea (a, b, c, d, e) ->
      textAreaAttributes a b c d
    _ ->
      []

textInputAttributes : Id -> ClassList -> Placeholder -> List (Html.Attribute a)
textInputAttributes inputId classList plac =
  let
    idx = Just (Html.Attributes.id ("input" ++ toString inputId))
    typex = Just (type' "text")
    classes = Just (class (String.join " " classList))
    placeholderx =
      case plac of
        Just pl -> Just (placeholder pl)
        Nothing -> Nothing
  in
    List.filterMap identity [idx, typex, classes, placeholderx]

textAreaAttributes : Id -> ClassList -> Placeholder -> RowNumber -> List (Html.Attribute a)
textAreaAttributes inputId classList plac rowNo =
  let
    -- idx = Just (Html.Attributes.id inputId)
    idx = Just (Html.Attributes.id ("input" ++ toString inputId))
    classes = Just (class (String.join " " classList))
    placeholderx =
      case plac of
        Just pl -> Just (placeholder pl)
        Nothing -> Nothing
    rowNox = Just (rows rowNo)
  in
    List.filterMap identity [idx, classes, placeholderx, rowNox]



selectHtml : Input -> Html Msg
selectHtml inp =
  text "ho"

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
