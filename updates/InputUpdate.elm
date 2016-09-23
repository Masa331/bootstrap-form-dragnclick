module InputUpdate exposing (..)

import Messages exposing (..)
import Models exposing (..)

update msg model =
  case msg of
    RemoveInput id ->
      removeInput model id
    EditInput id ->
      ({ model | currentlyEddited = Just id }, Cmd.none)
    StopEditing ->
      ({ model | currentlyEddited = Nothing }, Cmd.none)
    PlaceholderEdit placeholder ->
      case model.currentlyEddited of
        Nothing ->
          (model, Cmd.none)
        Just id ->
          updatePlaceholder id model placeholder

-------------
-- Private --
-------------

removeInput model inputId =
  let
    newElement = removeElement model inputId
  in
    ({ model | element = newElement }, Cmd.none)

updatePlaceholder id model placeholder =
  let
    element = model.element
    childs = (\ (Children childs) -> childs) element.children
    elementToUpdate = List.filter (\element -> element.id == id) childs |> List.head
    placeholderAttibute = Attribute "placeholder" placeholder
  in
    case elementToUpdate of
      Nothing ->
        (model, Cmd.none)
      Just element ->
        (model, Cmd.none)
