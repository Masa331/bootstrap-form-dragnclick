module Form exposing (view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import String exposing (..)

import Models exposing (..)
import Messages exposing (..)

view : Element -> List (Html Msg)
view htmlTree =
  toElmHtmlNode htmlTree

-------------
-- Private --
-------------

toElmHtmlNode : Element -> List (Html Msg)
toElmHtmlNode htmlTree =
  let
    attributes = createAttributes htmlTree
    childs = (\ (Children childs) -> childs) htmlTree.children
    value = Html.text htmlTree.value
    node = Html.node htmlTree.tag attributes
  in
    case childs of
      [] ->
        if htmlTree.tag == "editLinks" then
          [editLinks htmlTree.value]
        else
          [node [value]]
      x::xs ->
        [node ((List.concat (List.map toElmHtmlNode childs)) ++ [value])]

editLinks id =
  let
    resolvedId = idToInt id
    l1 = a [href "javascript:void(0);", onClick (FormMessage (EditInput resolvedId))] [text "Edit"]
    l2 = a [href "javascript:void(0);", onClick (FormMessage (RemoveInput resolvedId))] [text "Remove"]
    l3 = a [href "javascript:void(0);", onClick (FormMessage (MoveUp resolvedId))] [text "Move up"]
    l4 = a [href "javascript:void(0);", onClick (FormMessage (MoveDown resolvedId))] [text "Move Down"]
  in
    div
      [ class "edit-and-remove-link" ]
      [ l1, text " | ", l2, text " | ", l3, text " | ", l4]

idToInt id =
  let
    castedId = toInt id
  in
    case castedId of
      Ok val -> val
      Err _ -> 1


createAttributes model =
  List.map createAttribute model.attributes

createAttribute attribute =
  Html.Attributes.attribute attribute.name attribute.value
