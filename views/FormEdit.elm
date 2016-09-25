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
  in
    formGroup [ label inp, input ]

textAreaHtml : Input -> Html Msg
textAreaHtml inp =
  let
    area = textarea (inputAttributes inp) []
  in
    formGroup [ label inp, area ]

-------------
-- Helpers --
-------------

formGroup : List (Html Msg) -> Html Msg
formGroup els =
  div [ class "form-group" ] els

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
