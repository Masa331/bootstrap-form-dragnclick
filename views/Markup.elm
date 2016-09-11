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
      [text (element model)]

element model =
  let
    childs = (\ (Children childs) -> childs) model.children
    -- children = List.map element model.children
  in
    case childs of
      [] ->
        "<" ++ model.tag ++ ">"  ++ model.value  ++ "</" ++ model.tag ++ ">"
      x::xs ->
        "<" ++ model.tag ++ ">"  ++ (element x)  ++ "</" ++ model.tag ++ ">"

-- element model =
--   case model.children of
--     [] ->
--       "<" ++ model.tag ++ ">"  ++ model.value  ++ "</" ++ model.tag ++ ">"
--     x::xs ->
--       "<" ++ model.tag ++ ">"  ++ (element x)  ++ "</" ++ model.tag ++ ">"

-- element model =
--   let
--     childs = (\ (Children childs) -> childs) model.children
--   in
--     String.join "-" (List.map (\child -> child.tag) childs)

-- element model =
--   if (List.length model.children) <= 0 then
--     model.tag ++ model.value ++ model.tag
--   else
    -- model.tag ++ String.join "-" (List.map (\child -> child.value) model.children) ++ model.tag
--     String.join "-" (List.map (\child -> child.tag) model.children)

-- element model =
--   if (List.length model.children) <= 0 then
--     "<" ++ model.tag ++ ">"  ++ model.value  ++ "</" ++ model.tag ++ ">"
--   else
--     "<" ++ model.tag ++ ">"  ++ model.value  ++ "</" ++ model.tag ++ ">"




-- element model =
--   let
--     children =
--       if (List.length model.children) < 1 then
--         [""]
--       else
--         List.map element model.children
--   in
--     "<" ++ model.tag ++ ">"  ++ (String.join "\n" children)  ++ "</" ++ model.tag ++ ">"
