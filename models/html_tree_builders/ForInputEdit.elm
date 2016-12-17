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
    divider = span " " [] [] []

    children = [ l4 ]
    links = span "" [Attribute "class" "hidden-inherit float-right one-rem-size"] [] children
    label = span (Maybe.withDefault "" input.label) [] [] []
  in
    legend "" [] [] [label, links]

toLinks : Int -> Maybe Node
toLinks value =
  let
    i1 = i "" [Attribute "class" "fa fa-font fa-small control-element"] [] []
    l1 = span "" [] [Html.Events.onClick ((Messages.InputMessage (Messages.SizeEdit value "small")))] [i1]
    i2 = i "" [Attribute "class" "fa fa-font fa-normal control-element"] [] []
    l2 = span "" [] [Html.Events.onClick ((Messages.InputMessage (Messages.SizeEdit value "normal")))] [i2]
    i3 = i "" [Attribute "class" "fa fa-font fa-big control-element"] [] []
    l3 = span "" [] [Html.Events.onClick ((Messages.InputMessage (Messages.SizeEdit value "large")))] [i3]

    i4 = i "" [Attribute "class" "fa fa-check control-element"] [] []
    l4 = span "" [] [Html.Events.onClick ((Messages.InputMessage (Messages.ToggleDisabled value)))] [i4]
    divider = span " " [] [] []

    children = [ l1, divider, l2, divider, l3, divider, l4 ]
  in
    Just (div "" [Attribute "class" "control-container hidden-block"] [] children)
