module YourForm exposing (view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import Models exposing (..)
import Messages exposing (..)

view model =
  List.append [heading] (form model.element)

-------------
-- Private --
-------------

debug model =
  div
    []
    [text (toString model)]

heading =
  h1
  []
  [ text "Your Form"
  , small [ class "text-muted" ] [ text "Drag to move, Right click to edit" ]
  ]

form model =
  [ hr [] []
  , element model
  ]

element model =
  let
    attributes = createAttributes model
    childs = (\ (Children childs) -> childs) model.children
    value = valueForElement model
  in
    case childs of
      [] ->
       Html.node model.tag attributes value
      x::xs ->
       Html.node model.tag attributes (List.append (List.map element childs) value)

valueForElement element =
  if element.tag == "div" then
    [Html.text element.value
    , editAndRemoveLink element
    ]
  else
    [Html.text element.value]

editAndRemoveLink element =
  div [class "edit-and-remove-link"] [editLink element, removeLink element]

editLink element =
  a [href "#", onClick (EditInput element.id)] [text "Edit"]

removeLink element =
  a [href "#", onClick (RemoveInput element.id)] [text "Remove"]

createAttributes model =
  List.map createAttribute model.attributes

createAttribute attribute =
  Html.Attributes.attribute attribute.name attribute.value
