module FormView exposing (view)

import Html exposing (..)
import Html.Attributes exposing (..)

import HtmlNode exposing (..)
import Form exposing (..)
import Inputs exposing (..)
import Messages exposing (..)

view : Node -> List (Html Msg)
view htmlTree =
  toElmHtmlNode htmlTree

-------------
-- Private --
-------------

toElmHtmlNode : HtmlNode.Node -> List (Html Msg)
toElmHtmlNode (Node node) =
  let
    attributes = List.map createAttribute node.attributes
    childs = node.children
    value = if node.value == "" then [] else [Html.text node.value]
    htmlNode = Html.node node.tag (attributes ++ node.events)
  in
    case childs of
      [] ->
        [htmlNode value]
      x::xs ->
        [htmlNode ((List.concat (List.map toElmHtmlNode childs)) ++ value)]

createAttribute : HtmlNode.Attribute -> Html.Attribute a
createAttribute attribute =
  if attribute.name == "rows" then
    Html.Attributes.rows (rowsToNumber attribute.value)
  else
    Html.Attributes.attribute attribute.name attribute.value
