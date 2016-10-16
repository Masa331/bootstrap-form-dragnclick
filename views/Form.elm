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
        [node [value]]
      x::xs ->
        [node ((List.concat (List.map toElmHtmlNode childs)) ++ [value])]

createAttributes : HtmlTree.Element -> List (Html.Attribute a)
createAttributes model =
  List.map createAttribute model.attributes

createAttribute : HtmlTree.Attribute -> Html.Attribute a
createAttribute attribute =
  if attribute.name == "rows" then
    Html.Attributes.rows (rowsToNumber attribute.value)
  else
    Html.Attributes.attribute attribute.name attribute.value
