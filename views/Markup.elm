module Markup exposing (view)

import Html exposing (..)
import Html.Attributes exposing (..)
import String
import Utils

import Models exposing (..)
import HtmlTree exposing (..)
import Messages exposing (..)

view : Element -> Html Msg
view tree =
  let
    cleanedTree = removeElementRecursively "editLinks" tree
  in
    pre
      []
      [ text (toElmHtmlNode 0 cleanedTree) ]

-------------
-- Private --
-------------

toElmHtmlNode : Int -> HtmlTree.Element -> String
toElmHtmlNode nestingLevel model =
  let
    childs = (\ (Children childs) -> childs) model.children
    subNodes = List.map (toElmHtmlNode (nestingLevel + 1)) childs
                |> String.join " "
    value = if model.value == "" then "" else "\n" ++ (indent (nestingLevel + 1)) ++ model.value
  in
   case childs of
     [] ->
      inputToMarkup nestingLevel model value
     x::xs ->
      inputToMarkup nestingLevel model (subNodes ++ value)

inputToMarkup : Int -> HtmlTree.Element -> String -> String
inputToMarkup nestingLevel htmlTree value =
   "\n"
   ++ indent nestingLevel ++ openingTag htmlTree

   ++ value

   ++ if isVoid htmlTree then "" else "\n" ++ indent nestingLevel ++ closingTag htmlTree

indent : Int -> String
indent level =
  String.repeat level "  "

openingTag : Element -> String
openingTag model =
  let
    tag = model.tag
    attributes = htmlAttributesString model.attributes
    value = [tag, attributes]
      |> Utils.compact
      |> String.join " "
  in
    "<" ++ value ++ ">"

htmlAttributesString : List HtmlTree.Attribute -> String
htmlAttributesString attributes =
  List.map htmlAttributeString attributes
    |> String.join " "

htmlAttributeString : HtmlTree.Attribute -> String
htmlAttributeString attribute =
  attribute.name ++ "=" ++ "\"" ++ attribute.value ++ "\""

closingTag : Element -> String
closingTag element =
  "</" ++ element.tag ++ ">"

isVoid : Element -> Bool
isVoid element =
  List.member element.tag voidElementsList

voidElementsList : List String
voidElementsList =
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
