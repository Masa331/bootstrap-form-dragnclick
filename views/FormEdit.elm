module FormEdit exposing (view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import String exposing (..)

import Models exposing (..)
import Messages exposing (..)

view : Model -> Html Msg
view model =
  let
    elements = List.map inputHtml model.form
  in
    Html.form [] elements

inputHtml : Input -> Html Msg
inputHtml input =
  case input of
    TextInput (a, b, c) ->
      textInputHtml input
    TextArea (a, b, c) ->
      textAreaHtml input
    Select ->
      selectHtml input
    Multiselect ->
      multiselectHtml input
    FileUpload ->
      fileUploadHtml input
    Radio ->
      radioHtml input
    Checkbox ->
      checkboxHtml input
    Button ->
      buttonHtml input




textInputHtml : Input -> Html Msg
textInputHtml inp =
  div
    [class "form-group"]
    [ label [ for "input1" ] [ text "lable" ]
    , input [ type' "text", class "form-control", id "input1", placeholder "hovno" ] []
    ]

textAreaHtml : Input -> Html Msg
textAreaHtml inp =
  div
    [class "form-group"]
    [ label [ for "input2" ] [ text "lable" ]
    , textarea [class "form-control", id "input2", rows 3 ] []
    ]

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

  -- text "ahoj"
  -- text "ahoj"

-- view model =
--   (form model.element)

-- -------------
-- -- Private --
-- -------------
--
-- form model =
--   element model
--
-- element model =
--   let
--     attributes = createAttributes model
--     childs = (\ (Children childs) -> childs) model.children
--     value = Html.text model.value
--   in
--   case childs of
--     [] ->
--       if isDeletable model then
--         [Html.node model.tag attributes [value]
--         , editAndRemoveLink model]
--       else
--         [Html.node model.tag attributes [value]]
--     x::xs ->
--       if isDeletable model then
--         [Html.node model.tag attributes ((List.concat (List.map element childs)) ++ [value])
--         , editAndRemoveLink model]
--       else
--         [Html.node model.tag attributes ((List.concat (List.map element childs)) ++ [value])]
--
-- editAndRemoveLink element =
--   div [class "edit-and-remove-link"] [editLink element, removeLink element]
--
-- editLink element =
--   a [href "javascript:void(0);", onClick (InputMessage (EditInput element.id))] [text "Edit"]
--
-- removeLink element =
--   a [href "javascript:void(0);", onClick (InputMessage (RemoveInput element.id))] [text "Remove"]
--
-- createAttributes model =
--   List.map createAttribute model.attributes
--
-- createAttribute attribute =
--   Html.Attributes.attribute attribute.name attribute.value
