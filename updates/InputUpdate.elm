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
    placeholderAttibute = Attribute "placeholder" placeholder
    elementsToUpdate = List.filter (\element -> element.id == id) childs
    updatedElements = List.map (\element -> { element | attributes = (updateAttributes element.attributes placeholderAttibute) }) elementsToUpdate

    newChilds = List.filter (\child -> child.id /= id) childs
      |> List.append updatedElements
      |> Children

    updatedElement = { element | children = newChilds }
  in
    ({ model | element = updatedElement }, Cmd.none)

updateAttributes attributes newAttributes =
  let
    toUpdate = List.filter (\attr -> attr.value /= "") [newAttributes]
  in
    List.filter (\attr -> attr.name /= "placeholder") attributes
      |> List.append toUpdate
