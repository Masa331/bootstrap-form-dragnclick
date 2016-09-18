module Markup exposing (view)

import Html exposing (..)
import Html.Attributes exposing (..)
import String

import Models exposing (..)

view model =
  [form model]

-------------
-- Private --
-------------

form model =
  let
    attributes = model.attributes
    children = model.children
  in
    pre
      []
      [text (element 0 model)]

element nestingLevel model =
  let
    childs = (\ (Children childs) -> childs) model.children
  in
    case childs of
      [] ->
       (String.repeat nestingLevel "  ") ++ (openingTag model)  ++ model.value  ++ (closingTag model)
      x::xs ->
        (String.repeat nestingLevel "  ") ++ (openingTag model)  ++ "\n" ++ (String.join "\n" (List.map (element (nestingLevel + 1)) childs)) ++ (valuePresence (nestingLevel + 1) model) ++ "\n" ++ (String.repeat nestingLevel "  ") ++ (closingTag model)

valuePresence nestingLevel model =
  if model.value /= "" then
    "\n" ++ (String.repeat nestingLevel "  ") ++ model.value
  else
    ""

openingTag model =
  let
    tag = model.tag
    attributes = htmlAttributesString model.attributes
  in
    String.join "" ["<", tag, attributes, ">"]

htmlAttributesString attributes =
  let
    stringifiedAttributes = (List.map htmlAttributeString attributes)
  in
    if List.length stringifiedAttributes > 0 then
      " " ++ (String.join " " stringifiedAttributes)
    else
      ""

htmlAttributeString attribute =
  if attribute.value == "" then
    attribute.name
  else
    attribute.name ++ "=" ++ "\"" ++ attribute.value ++ "\""

closingTag element =
  if Models.isVoid element then
     ""
  else
    "</" ++ element.tag ++ ">"
