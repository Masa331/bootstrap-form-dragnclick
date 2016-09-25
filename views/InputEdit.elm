module InputEdit exposing (view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import String exposing (..)

import Models exposing (..)
import Messages exposing (..)

view model =
  [div [] [text "ahoj"]]
--   let
--     childs = (\ (Children childs) -> childs) model.element.children
--     input = List.head (List.filter (\el -> (Just el.id) == model.currentlyEddited) childs)
--   in
--     case input of
--       Nothing ->
--         [div [] []]
--       Just a ->
--         element a
--
-- -------------
-- -- Private --
-- -------------
--
-- element model =
--   let
--     attributes = createAttributes model
--     childs = (\ (Children childs) -> childs) model.children
--     value = Html.text model.value
--   in
--   case childs of
--     [] ->
--       [Html.node model.tag attributes [value]]
--     x::xs ->
--       [Html.node model.tag attributes ((List.concat (List.map element childs)) ++ [value])]
--
-- createAttributes model =
--   List.map createAttribute model.attributes
--
-- createAttribute attribute =
--   Html.Attributes.attribute attribute.name attribute.value
