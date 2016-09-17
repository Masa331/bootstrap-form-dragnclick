module Updates exposing (..)

import Messages exposing (..)
import Models exposing (..)

update msg model =
  case msg of
    AddTextInput ->
      -- addInputToForm model (textInput generateNextId)
      addInputToForm model (textInput (generateNextId model))
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
    -- newId = generateNextId model
    -- input = { input | id = input.id }
    element = { oldElement | children = Children (List.append [input] childs) }
  in
    ({ model | element = element, currentId = input.id }, Cmd.none)


