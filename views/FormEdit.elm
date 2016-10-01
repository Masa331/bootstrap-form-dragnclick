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
        if isDeletable model then
          [editLinks model.value]
        else
          [Html.node model.tag attributes [value]]
      x::xs ->
        if isDeletable model then
          [Html.node model.tag attributes ((List.concat (List.map toElmHtmlNode childs)) ++ [value])]
        else
          [Html.node model.tag attributes ((List.concat (List.map toElmHtmlNode childs)) ++ [value])]

editLinks id =
  let
    castedId = toInt id
    resolvedId =
      case castedId of
        Ok val -> val
        Err _ -> 1
    l1 = a [href "javascript:void(0);", onClick (FormMessage (EditInput resolvedId))] [text "Edit"]
    l2 = a [href "javascript:void(0);", onClick (FormMessage (RemoveInput resolvedId))] [text "Remove"]
    l3 = a [href "javascript:void(0);"] [text "Move up"]
    l4 = a [href "javascript:void(0);"] [text "Move Down"]
  in
    div
      [ class "edit-and-remove-link" ]
      [ l1, text " | ", l2, text " | ", l3, text " | ", l4]



createAttributes model =
  List.map createAttribute model.attributes

createAttribute attribute =
  -- if True then
  --   Html.Attributes.attribute "disabled" True
  -- else
    Html.Attributes.attribute attribute.name attribute.value
  --
  -- case attribute.name of
  --   "disabled" -> Html.Attributes.disabled True
  --   _ -> Html.Attributes.attribute attribute.name attribute.value
  --   -- "disabled" -> Html.Attributes.disabled True
  --   -- _ -> Html.Attributes.attribute attribute.name attribute.value
