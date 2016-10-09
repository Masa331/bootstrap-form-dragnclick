module Markup exposing (view)

import Html exposing (..)
import Html.Attributes exposing (..)
import String

import Models exposing (..)
import HtmlTree exposing (..)
import Messages exposing (..)

view : Element -> Html Msg
view htmlTree =
  pre
    []
    [ text (toElmHtmlNode 0 htmlTree) ]

-------------
-- Private --
-------------

toElmHtmlNode : Int -> HtmlTree.Element -> String
toElmHtmlNode nestingLevel model =
  let
    childs = (\ (Children childs) -> childs) model.children
  in
   case childs of
     [] ->
       (String.repeat nestingLevel "  ") ++ (openingTag model)  ++ model.value  ++ (closingTag model)
     x::xs ->
       (String.repeat nestingLevel "  ") ++ (openingTag model)  ++ "\n" ++ (String.join "\n" (List.map (toElmHtmlNode (nestingLevel + 1)) childs)) ++ (valuePresence (nestingLevel + 1) model) ++ "\n" ++ (String.repeat nestingLevel "  ") ++ (closingTag model)

valuePresence nestingLevel model =
  if model.value /= "" then
    "\n" ++ (String.repeat nestingLevel "  ") ++ model.value
  else
    ""

openingTag : Element -> String
openingTag model =
  let
    tag = model.tag
    attributes = htmlAttributesString model.attributes
  in
    String.join "" ["<", tag, attributes, ">"]

htmlAttributesString : List HtmlTree.Attribute -> String
htmlAttributesString attributes =
  let
    stringifiedAttributes = (List.map htmlAttributeString attributes)
  in
    if List.length stringifiedAttributes > 0 then
      " " ++ (String.join " " stringifiedAttributes)
    else
      ""

htmlAttributeString : HtmlTree.Attribute -> String
htmlAttributeString attribute =
  if attribute.value == "" then
    attribute.name
  else
    attribute.name ++ "=" ++ "\"" ++ attribute.value ++ "\""

closingTag : Element -> String
closingTag element =
  if isVoid element then
    ""
  else
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
