module YourForm exposing (view)

import Html exposing (..)
import Html.Attributes exposing (..)

import Models exposing (..)

view model =
  List.append [heading] (form model)


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
  [debug model
  , hr [] []
  , element model
  ]

element model =
  let
    attributes = createAttributes model
    childs = (\ (Children childs) -> childs) model.children
    value = Html.text model.value
  in
    case childs of
      [] ->
       Html.node model.tag attributes [value]
      x::xs ->
       Html.node model.tag attributes (List.append (List.map element childs) [value])

createAttributes model =
  List.map createAttribute model.attributes

createAttribute attribute =
  Html.Attributes.attribute attribute.name attribute.value
