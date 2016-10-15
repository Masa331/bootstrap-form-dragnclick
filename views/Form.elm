module Form exposing (view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import String exposing (..)

import Models exposing (..)
import HtmlTree exposing (..)
import FormModel exposing (..)
import Messages exposing (..)

view : Element -> List (Html Msg)
view htmlTree =
  toElmHtmlNode htmlTree

-------------
-- Private --
-------------

toElmHtmlNode : HtmlTree.Element -> List (Html Msg)
toElmHtmlNode htmlTree =
  let
    attributes = createAttributes htmlTree
    childs = (\ (HtmlTree.Children childs) -> childs) htmlTree.children
    value = Html.text htmlTree.value
    node = Html.node htmlTree.tag (attributes ++ htmlTree.events)
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

createAttributes : HtmlTree.Element -> List (Html.Attribute a)
createAttributes model =
  List.map createAttribute model.attributes

createAttribute : HtmlTree.Attribute -> Html.Attribute a
createAttribute attribute =
  if attribute.name == "rows" then
    Html.Attributes.rows (rowsToNumber attribute.value)
  else
    Html.Attributes.attribute attribute.name attribute.value
