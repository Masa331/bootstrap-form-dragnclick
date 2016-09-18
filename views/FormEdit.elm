module FormEdit exposing (view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import String exposing (..)

import Models exposing (..)
import Messages exposing (..)

view model =
  (form model.element)

-------------
-- Private --
-------------

form model =
  element model

element model =
  let
    attributes = createAttributes model
    childs = (\ (Children childs) -> childs) model.children
    value = Html.text model.value
  in
  case childs of
    [] ->
      if isDeletable model then
        [Html.node model.tag attributes [value]
        , editAndRemoveLink model]
      else
        [Html.node model.tag attributes [value]]
    x::xs ->
      if isDeletable model then
        [Html.node model.tag attributes ((List.concat (List.map element childs)) ++ [value])
        , editAndRemoveLink model]
      else
        [Html.node model.tag attributes ((List.concat (List.map element childs)) ++ [value])]

editAndRemoveLink element =
  div [class "edit-and-remove-link"] [editLink element, removeLink element]

editLink element =
  a [href "javascript:void(0);", onClick (InputMessage (EditInput element.id))] [text "Edit"]

removeLink element =
  a [href "javascript:void(0);", onClick (InputMessage (RemoveInput element.id))] [text "Remove"]

createAttributes model =
  List.map createAttribute model.attributes

createAttribute attribute =
  Html.Attributes.attribute attribute.name attribute.value
