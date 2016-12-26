module ForInputEdit exposing (build)

import HtmlNode exposing (..)
import Inputs exposing (..)
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
    containerClass = Attribute "class" "form-group show-hidden-on-hover"
    links = toLinks input
  in
    Bootstrap.textInputToHtmlNode input containerClass links

colorToHtmlTree input =
  let
    containerClass = Attribute "class" "form-group show-hidden-on-hover"
    links = toLinks input
  in
    Bootstrap.colorToHtmlNode input containerClass links

selectToHtmlTree input =
  let
    containerClass = Attribute "class" "form-group show-hidden-on-hover"
    links = toLinks input
  in
    Bootstrap.selectToHtmlNode input containerClass links

textAreaToHtmlTree input =
  let
    containerClass = Attribute "class" "form-group show-hidden-on-hover"
    links = toLinks input
  in
    Bootstrap.textAreaToHtmlNode input containerClass links

multiselectToHtmlTree input =
  let
    containerClass = Attribute "class" "form-group show-hidden-on-hover"
    links = toLinks input
  in
    Bootstrap.multiselectToHtmlNode input containerClass links

fileUploadToHtmlTree input =
  let
    containerClass = Attribute "class" "form-group show-hidden-on-hover"
    links = toLinks input
  in
    Bootstrap.fileUploadToHtmlNode input containerClass links

radioToHtmlTree input =
  let
    legend = Just (toLegend input)
    containerClass = Attribute "class" "form-group show-hidden-on-hover"
  in
    Bootstrap.radioToHtmlNode input containerClass legend

checkboxToHtmlTree input =
  let
    containerClass = Attribute "class" "form-check show-hidden-on-hover"
    checkboxLinks =
      div "" [Attribute "class" "control-container hidden-block"] [] [ Bootstrap.disabledLink input ]
  in
    Bootstrap.checkboxToHtmlNode input containerClass checkboxLinks

buttonToHtmlTree input =
  let
    links = toLinks input
    containerClass = Attribute "class" "form-group show-hidden-on-hover"
  in
    Bootstrap.buttonToHtmlNode input containerClass links

-------------
-- Helpers --
-------------

toLegend : Input -> Node
toLegend input =
  let
    children = [ Bootstrap.disabledLink input ]
    links = span "" [Attribute "class" "hidden-inherit float-right one-rem-size"] [] children
    label = span (Maybe.withDefault "" input.label) [] [] []
  in
    legend "" [] [] [label, links]

toLinks : Input -> Node
toLinks input =
  let
    links =
      [ Bootstrap.sizeLinkSmall input
      , Bootstrap.sizeLinkNormal input
      , Bootstrap.sizeLinkLarge input
      , Bootstrap.disabledLink input
      ] |> List.intersperse Bootstrap.linksDivider
  in
    div "" [Attribute "class" "control-container hidden-block"] [] links
