module MarkupView exposing (view)

import Html exposing (..)
import Html.Attributes exposing (..)
import String
import Utils

import Models exposing (..)
import HtmlNode exposing (..)
import Messages exposing (..)

view : Node -> Html Msg
view tree =
  pre
    []
    [ text (toElmHtmlNode 0 tree) ]

-------------
-- Private --
-------------

toElmHtmlNode : Int -> HtmlNode.Node -> String
toElmHtmlNode nestingLevel (Node node) =
  let
    childs = node.children
    subNodes = List.map (toElmHtmlNode (nestingLevel + 1)) childs
                |> String.join " "
    value = if node.value == "" then "" else "\n" ++ (indent (nestingLevel + 1)) ++ node.value
  in
   case childs of
     [] ->
      inputToMarkup nestingLevel node value
     x::xs ->
      inputToMarkup nestingLevel node (subNodes ++ value)

inputToMarkup : Int -> NodeAttrs -> String -> String
inputToMarkup nestingLevel node value =
   "\n"
   ++ indent nestingLevel ++ openingTag node

   ++ value

   ++ if isVoid node then "" else "\n" ++ indent nestingLevel ++ closingTag node

indent : Int -> String
indent level =
  String.repeat level "  "

openingTag : NodeAttrs -> String
openingTag node =
  let
    tag = node.tag
    attributes = htmlAttributesString node.attributes
    value = [tag, attributes]
      |> Utils.compact
      |> String.join " "
  in
    "<" ++ value ++ ">"

htmlAttributesString : List HtmlNode.Attribute -> String
htmlAttributesString attributes =
  List.map htmlAttributeString attributes
    |> String.join " "

htmlAttributeString : HtmlNode.Attribute -> String
htmlAttributeString attribute =
  attribute.name ++ "=" ++ "\"" ++ attribute.value ++ "\""

closingTag : NodeAttrs -> String
closingTag node =
  "</" ++ node.tag ++ ">"

isVoid : NodeAttrs -> Bool
isVoid node =
  List.member node.tag voidNodesList

voidNodesList : List String
voidNodesList =
  [ "area"
  , "base"
  , "br"
  , "col"
  , "command"
  , "embed"
  , "hr"
  , "img"
  , "input"
  , "keygen"
  , "link"
  , "meta"
  , "param"
  , "source"
  , "track"
  , "wbr" ]
