module ForInputEdit exposing (build)

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
    containerClass = Attribute "class" "form-group show-hidden-on-hover"
    links = toLinks input.id
  in
    Bootstrap.textInputToHtmlNode input containerClass links

colorToHtmlTree input =
  let
    containerClass = Attribute "class" "form-group show-hidden-on-hover"
    links = toLinks input.id
  in
    Bootstrap.colorToHtmlNode input containerClass links

selectToHtmlTree input =
  let
    containerClass = Attribute "class" "form-group show-hidden-on-hover"
    links = toLinks input.id
  in
    Bootstrap.selectToHtmlNode input containerClass links

textAreaToHtmlTree input =
  let
    containerClass = Attribute "class" "form-group show-hidden-on-hover"
    links = toLinks input.id
  in
    Bootstrap.textAreaToHtmlNode input containerClass links

multiselectToHtmlTree input =
  let
    containerClass = Attribute "class" "form-group show-hidden-on-hover"
    links = toLinks input.id
  in
    Bootstrap.multiselectToHtmlNode input containerClass links

fileUploadToHtmlTree input =
  let
    containerClass = Attribute "class" "form-group show-hidden-on-hover"
    links = toLinks input.id
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
    links = toLinks input.id
    containerClass = Attribute "class" "form-check show-hidden-on-hover"
  in
    Bootstrap.checkboxToHtmlNode input containerClass links

buttonToHtmlTree input =
  let
    links = toLinks input.id
    containerClass = Attribute "class" "form-group show-hidden-on-hover"
  in
    Bootstrap.buttonToHtmlNode input containerClass links

-------------
-- Helpers --
-------------

toLegend : Input -> Node
toLegend input =
  let
    i4 = i "" [Attribute "class" "fa fa-check control-element"] [] []
    l4 = span "" [] [Html.Events.onClick ((Messages.InputMessage (Messages.ToggleDisabled input.id)))] [i4]

    children = [ l4 ]
    links = span "" [Attribute "class" "hidden-inherit float-right one-rem-size"] [] children
    label = span (Maybe.withDefault "" input.label) [] [] []
  in
    legend "" [] [] [label, links]

toLinks : Int -> Node
toLinks value =
  let
    l1 = iconLink "fa-font fa-small" (Messages.InputMessage (Messages.SizeEdit value "small"))
    l2 = iconLink "fa-font fa-normal" (Messages.InputMessage (Messages.SizeEdit value "normal"))
    l3 = iconLink "fa-font fa-big" (Messages.InputMessage (Messages.SizeEdit value "large"))
    l4 = iconLink "fa-check" (Messages.InputMessage (Messages.ToggleDisabled value))
    divider = span " " [] [] []

    children = [ l1, divider, l2, divider, l3, divider, l4 ]
  in
    div "" [Attribute "class" "control-container hidden-block"] [] children

iconLink : String -> Messages.Msg -> Node
iconLink class msg =
  let
    icon = i "" [Attribute "class" ("fa control-element " ++ class)] [] []
  in
    span "" [] [Html.Events.onClick (msg)] [icon]
