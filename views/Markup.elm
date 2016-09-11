module Markup exposing (view)

import Html exposing (..)
import Html.Attributes exposing (..)
import String

import Models exposing (..)

view model =
  List.append [heading] [form model]

heading =
  h1
   []
   [ text "Markup"
   , small [ class "text-muted" ] [ text "Copy and paste to your page" ]
   ]

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
        (openingTag model)  ++ model.value  ++ (closingTag model)
      x::xs ->
        (openingTag model)  ++ "\n" ++ "  " ++ (String.join "\n" (List.map (element (nestingLevel + 1)) childs)) ++ "\n" ++ (closingTag model)

openingTag model =
  "<" ++ model.tag ++ ">"

closingTag model =
  "</" ++ model.tag ++ ">"

-- element model =
--   let
--     children =
--       if (List.length model.children) < 1 then
--         [""]
--       else
--         List.map element model.children
--   in
--     "<" ++ model.tag ++ ">"  ++ (String.join "\n" children)  ++ "</" ++ model.tag ++ ">"
