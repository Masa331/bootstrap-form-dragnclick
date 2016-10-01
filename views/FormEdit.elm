module FormEdit exposing (view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import String exposing (..)

import Models exposing (..)
import Messages exposing (..)

import Inputs exposing (..)

view : Model -> Html Msg
view model =
  let
    elements = List.map inputHtml model.form
  in
    -- Html.form [] elements
    Html.form [] (toElmHtmlNode (modelToHtmlTree model.form))

toElmHtmlNode model =
  let
    attributes = createAttributes model
    childs = (\ (Children childs) -> childs) model.children
    value = Html.text model.value
  in
    case childs of
      [] ->
        if False then
          [Html.node model.tag attributes [value]]
        else
          [Html.node model.tag attributes [value]]
      x::xs ->
        if False then
          [Html.node model.tag attributes ((List.concat (List.map toElmHtmlNode childs)) ++ [value])]
        else
          [Html.node model.tag attributes ((List.concat (List.map toElmHtmlNode childs)) ++ [value])]

createAttributes model =
  List.map createAttribute model.attributes

createAttribute attribute =
  Html.Attributes.attribute attribute.name attribute.value
