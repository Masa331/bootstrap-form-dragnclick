module Dragged exposing (build)

import Html.Events
import HtmlNode exposing (..)
import Inputs exposing (..)
import Messages
import Bootstrap

build : Input -> HtmlNode.Node
build input =
  case input.type_ of
    Text -> textInputToHtmlTree input
    TextArea -> textAreaToHtmlTree input
    Select -> selectToHtmlTree input
    Multiselect -> multiselectToHtmlTree input
    FileUpload -> fileUploadToHtmlTree input
    Radio -> radioToHtmlTree input
    Checkbox -> checkboxToHtmlTree input
    Button -> buttonToHtmlTree input
    Color -> colorToHtmlTree input
    _ -> textInputToHtmlTree input

-------------
-- Private --
-------------

textInputToHtmlTree input =
  let
    containerClass = Attribute "class" "form-group dragged"
    links = toLinks input.id
  in
    Bootstrap.textInputToHtmlNode input containerClass links

colorToHtmlTree input =
  let
    containerClass = Attribute "class" "form-group dragged"
    links = toLinks input.id
  in
    Bootstrap.colorToHtmlNode input containerClass links

selectToHtmlTree input =
  let
    containerClass = Attribute "class" "form-group dragged"
    links = toLinks input.id
  in
    Bootstrap.selectToHtmlNode input containerClass links

textAreaToHtmlTree input =
  let
    containerClass = Attribute "class" "form-group dragged"
    links = toLinks input.id
  in
    Bootstrap.textAreaToHtmlNode input containerClass links

multiselectToHtmlTree input =
  let
    containerClass = Attribute "class" "form-group dragged"
    links = toLinks input.id
  in
    Bootstrap.multiselectToHtmlNode input containerClass links

fileUploadToHtmlTree input =
  let
    containerClass = Attribute "class" "form-group dragged"
    links = toLinks input.id
  in
    Bootstrap.fileUploadToHtmlNode input containerClass links

radioToHtmlTree input =
  let
    legend = toLegend input.label
    containerClass = Attribute "class" "form-group dragged"
  in
    Bootstrap.radioToHtmlNode input containerClass legend

checkboxToHtmlTree input =
  let
    links = toLinks input.id
    containerClass = Attribute "class" "form-check dragged"
  in
    Bootstrap.checkboxToHtmlNode input containerClass links

buttonToHtmlTree input =
  let
    links = toLinks input.id
    containerClass = Attribute "class" "my-container dragged"
  in
    Bootstrap.buttonToHtmlNode input containerClass links

-------------
-- Helpers --
-------------

toLegend : Maybe String -> Maybe Node
toLegend value =
  Maybe.map (\value -> legend value [] [] []) value

toLinks : Int -> Node
toLinks value =
  div "" [Attribute "class" "control-container"] []
    [i "" [Attribute "class" "fa fa-arrows control-element"] [] []]
