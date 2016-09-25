module InputUpdate exposing (..)

import Messages exposing (..)
import Models exposing (..)

-- update msg model =
--   (model, Cmd.none)

update msg model =
  case msg of
    PlaceholderEdit placeholder ->
      case model.currentlyEdditedInputId of
        Nothing ->
          (model, Cmd.none)
        Just id ->
          updatePlaceholder id model placeholder
    NoOp ->
      (model, Cmd.none)
--
-- -------------
-- -- Private --
-- -------------
--

updatePlaceholder id model newPlaceholder =
  let
    input = List.head (List.filter (\el -> extractId el == id) model.form)
    newInputs = List.map (\inp -> if extractId inp == id then updateInputPlaceholder inp newPlaceholder else inp) model.form
    -- element = model.element
    -- elementsToUpdate = List.filter (\element -> element.id == id) childs
  in
    ({ model | form = newInputs }, Cmd.none)
    -- case input of
    --   Nothing ->
    --     (model, Cmd.none)
    --   Just inp ->
    --     (model, Cmd.none)

updateInputPlaceholder inp newPlaceholder =
  case inp of
    TextInput (a, b, c, d) ->
      TextInput (a, b, Just newPlaceholder, d)
    _ -> inp

--
-- removeInput model inputId =
--   let
--     newElement = removeElement model inputId
--   in
--     ({ model | element = newElement }, Cmd.none)
--
-- updatePlaceholders id model placeholder =
--   let
--     element = model.element
--     childs = (\ (Children childs) -> childs) element.children
--     placeholderAttibute = Attribute "placeholder" placeholder
--     elementsToUpdate = List.filter (\element -> element.id == id) childs
--     updatedElements = List.map (\element -> { element | attributes = (updateAttributes element.attributes placeholderAttibute) }) elementsToUpdate
--
--     newChilds = List.filter (\child -> child.id /= id) childs
--       |> List.append updatedElements
--       |> Children
--
--     updatedElement = { element | children = newChilds }
--   in
--     ({ model | element = updatedElement }, Cmd.none)
--
-- updateAttributes attributes newAttributes =
--   let
--     toUpdate = List.filter (\attr -> attr.value /= "") [newAttributes]
--   in
--     List.filter (\attr -> attr.name /= "placeholder") attributes
--       |> List.append toUpdate
