module Updates exposing (..)

import Messages exposing (..)
import Models exposing (..)

update msg model =
  case msg of
    AddTextInput ->
      addInputToForm model (textInput (generateNextId model))
    AddSelect ->
      addInputToForm model (select (generateNextId model))
    AddMultiselect ->
      addInputToForm model (multiselect (generateNextId model))
    AddTextarea ->
      addInputToForm model (textarea (generateNextId model))
    AddFileUpload ->
      addInputToForm model (fileUpload (generateNextId model))
    AddRadioButtons ->
      addInputToForm model (radioButtons (generateNextId model))
    AddCheckbox ->
      addInputToForm model (checkbox (generateNextId model))
    AddButton ->
      addInputToForm model (button (generateNextId model))
    RemoveInput id ->
      removeInput model id
    EditInput id ->
      (model, Cmd.none)

-------------
-- Private --
-------------

removeInput model inputId =
  let
    newElement = removeElementsRecursive model inputId
  in
    ({ model | element = newElement }, Cmd.none)


addInputToForm model input =
  let
    oldElement = model.element
    childs = (\ (Children childs) -> childs) model.element.children
    element = { oldElement | children = Children (List.append [input] childs) }
  in
    ({ model | element = element, currentId = input.id }, Cmd.none)


