module Markup exposing (view, inputView)

import Html exposing (..)
import Html.Attributes exposing (..)
import String

import Models exposing (..)
import Messages exposing (..)

view : Model -> Html Msg
view model =
  let
    inputs = List.map inputMarkup model.form
    inputsString = String.join "\n" inputs
  in
    pre
      []
      [ text ("<form>\n" ++ inputsString ++ "\n</form>") ]

inputView : Input -> Html Msg
inputView inp =
  pre
    []
    [ text ("<form>\n" ++ inputMarkup inp ++ "\n</form>") ]

inputMarkup : Input -> String
inputMarkup el =
  case el of
    TextInput (a, b, c, _) ->
      textInputCode el
    TextArea (_, _, _, _, _) ->
      textAreaCode el
    _ ->
      textInputCode el

textInputCode : Input -> String
textInputCode inp =
  let
    line1 = "  <div class=\"form-group\">"
    line2 = "    <label for=\"input1\">Input 1</label>"
    line3 = "    <input type=\"text\" class=\"form-control\" id=\"input1\" placeholder=\"neco\">"
    line4 = "  </div>"
  in
    String.join "\n" [line1, line2, line3, line4]


textAreaCode : Input -> String
textAreaCode inp =
  let
    line1 = "  <div class=\"form-group\">"
    line2 = "    <label for=\"input2\">Input 2</label>"
    line3 = "    <textarea class=\"form-control\" id=\"input2\" rows=3>"
    line4 = "  </div>"
  in
    String.join "\n" [line1, line2, line3, line4]

--
-- -------------
-- -- Private --
-- -------------
--
-- form model =
--   let
--     attributes = model.attributes
--     children = model.children
--   in
--     pre
--       []
--       [text (element 0 model)]
--
-- element nestingLevel model =
--   let
--     childs = (\ (Children childs) -> childs) model.children
--   in
--     case childs of
--       [] ->
--        (String.repeat nestingLevel "  ") ++ (openingTag model)  ++ model.value  ++ (closingTag model)
--       x::xs ->
--         (String.repeat nestingLevel "  ") ++ (openingTag model)  ++ "\n" ++ (String.join "\n" (List.map (element (nestingLevel + 1)) childs)) ++ (valuePresence (nestingLevel + 1) model) ++ "\n" ++ (String.repeat nestingLevel "  ") ++ (closingTag model)
--
-- valuePresence nestingLevel model =
--   if model.value /= "" then
--     "\n" ++ (String.repeat nestingLevel "  ") ++ model.value
--   else
--     ""
--
-- openingTag model =
--   let
--     tag = model.tag
--     attributes = htmlAttributesString model.attributes
--   in
--     String.join "" ["<", tag, attributes, ">"]
--
-- htmlAttributesString attributes =
--   let
--     stringifiedAttributes = (List.map htmlAttributeString attributes)
--   in
--     if List.length stringifiedAttributes > 0 then
--       " " ++ (String.join " " stringifiedAttributes)
--     else
--       ""
--
-- htmlAttributeString attribute =
--   if attribute.value == "" then
--     attribute.name
--   else
--     attribute.name ++ "=" ++ "\"" ++ attribute.value ++ "\""
--
-- closingTag element =
--   if Models.isVoid element then
--      ""
--   else
--     "</" ++ element.tag ++ ">"
