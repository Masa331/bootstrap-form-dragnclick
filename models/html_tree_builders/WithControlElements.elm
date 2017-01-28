module WithControlElements exposing (build)

import HtmlNode exposing (..)
import Form exposing (..)
import Inputs exposing (..)
import Bootstrap

build : Form.Row -> List HtmlNode.Node
build row =
  List.map buildInput row

buildInput : Input -> HtmlNode.Node
buildInput input =
  let
    links = toLinks input
    containerClass =
      Attribute "class" ("form-group show-hidden-on-hover" ++ if input.dragged then " hidden" else "")
  in
    case input.type_ of
      Text ->
        Bootstrap.textInputToHtmlNode input containerClass links
      TextArea ->
        Bootstrap.textAreaToHtmlNode input containerClass links
      Select ->
        Bootstrap.selectToHtmlNode input containerClass links
      Multiselect ->
        Bootstrap.multiselectToHtmlNode input containerClass links
      FileUpload ->
        Bootstrap.fileUploadToHtmlNode input containerClass links
      Radio ->
        Bootstrap.radioToHtmlNode input containerClass (Just (toLegend input))
      Checkbox ->
        let
          checkboxContainerClass = (Attribute "class" ("form-check show-hidden-on-hover" ++ if input.dragged then " hidden" else ""))
          checkboxLinks =
            [ Bootstrap.editLink input
            , Bootstrap.deleteLink input
            , Bootstrap.disabledLink input
            , Bootstrap.dragLink input
            ] |> List.intersperse Bootstrap.linksDivider
              |> div "" [Attribute "class" "control-container hidden-block"] []
        in
          Bootstrap.checkboxToHtmlNode input checkboxContainerClass checkboxLinks
      Button ->
        let
          buttonContainerClass = Attribute "class" ("my-container show-hidden-on-hover" ++ if input.dragged then " hidden" else "")
        in
          Bootstrap.buttonToHtmlNode input buttonContainerClass links
      Color ->
        Bootstrap.colorToHtmlNode input containerClass links
      Password ->
        Bootstrap.textInputToHtmlNode input containerClass links
      Number ->
        Bootstrap.textInputToHtmlNode input containerClass links
      DatetimeLocal ->
        Bootstrap.textInputToHtmlNode input containerClass links
      Date ->
        Bootstrap.textInputToHtmlNode input containerClass links
      Time ->
        Bootstrap.textInputToHtmlNode input containerClass links

-------------
-- Private --
-------------

toLegend : Input -> Node
toLegend input =
  let
    children =
      [ Bootstrap.editLink input
      , Bootstrap.deleteLink input
      , Bootstrap.disabledLink input
      , Bootstrap.dragLink input
      ] |> List.intersperse Bootstrap.linksDivider

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
      , Bootstrap.editLink input
      , Bootstrap.deleteLink input
      , Bootstrap.disabledLink input
      , Bootstrap.dragLink input
      ] |> List.intersperse Bootstrap.linksDivider
  in
    div "" [Attribute "class" "control-container hidden-block"] [] links
