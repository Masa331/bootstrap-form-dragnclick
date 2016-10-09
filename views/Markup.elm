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
       if model.tag == "editLinks" then
         ""
       else
         indent nestingLevel ++ wrapInTags model model.value
     x::xs ->
       let
         transformedChilds =
           "\n" ++ (String.join "\n" (List.map (toElmHtmlNode (nestingLevel + 1)) childs))
           ++ if model.value /= "" then "\n" ++ indent (nestingLevel + 1) ++ model.value ++ "\n" ++ indent (nestingLevel) else ""
           ++ "\n" ++ indent nestingLevel
       in
         indent nestingLevel ++ wrapInTags model transformedChilds

indent : Int -> String
indent level =
  String.repeat level "  "

wrapInTags element content =
  (openingTag element) ++ content  ++ (closingTag element)

openingTag : Element -> String
openingTag model =
  let
    tag = model.tag
    attributes = htmlAttributesString model.attributes
  in
    "<" ++ tag ++ attributes ++ ">"

htmlAttributesString : List HtmlTree.Attribute -> String
htmlAttributesString attributes =
  let
    stringifiedAttributes = (List.map htmlAttributeString attributes)
  in
    if List.length attributes > 0 then
      " " ++ (String.join " " stringifiedAttributes)
    else
      ""

htmlAttributeString : HtmlTree.Attribute -> String
htmlAttributeString attribute =
  if attribute.value == "" then
    attribute.name
  else
    attribute.name ++ "=" ++ "\"" ++ (Debug.log "atr" attribute.value) ++ "\""

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
